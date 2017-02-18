{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Backends.CStatic where

import Backends.Backend (Backend(..))
import Backends.SmudgeIR (
  SmudgeIR,
  Def(..),
  Dec(..),
  Stmt(..),
  Expr(..),
  Lit(..),
  lower,
  lowerSymTab,
  )
import Grammars.Smudge (
  Annotated(..),
  StateMachineDeclarator(..),
  )
import Grammars.C89
import Model (
  QualifiedName,
  Qualifiable(qualify),
  TaggedName(..),
  mangleWith,
  )
import Semantics.Solver (
  Ty(..), 
  Binding(..), 
  insertFunctions,
  filterBind,
  (!),
  )
import Semantics.Alias (Alias, rename)
import Trashcan.FilePath (relPath)
import Trashcan.These (
  These(..),
  maybeThis,
  maybeThat,
  fmapThis,
  theseAndThat,
  )
import Unparsers.C89 (renderPretty)

import Control.Arrow (first, second, (***), (&&&))
import Control.Monad (liftM)
import Control.Monad.State (StateT, evalState, state)
import Data.List (dropWhileEnd)
import System.Console.GetOpt
import System.FilePath (
  FilePath,
  dropExtension,
  takeDirectory,
  normalise,
  takeFileName,
  (<.>)
  )

data CStaticOption = OutFile FilePath
                   | Header FilePath
                   | ExtFile FilePath
                   | NoDebug
    deriving (Show, Eq)

(+-+) :: Identifier -> Identifier -> Identifier
a +-+ b = dropWhileEnd (== '_') a ++ "_" ++ dropWhile (== '_') b

makeSwitch :: Expression -> [(ConstantExpression, [Expression])] -> [Expression] -> Statement
makeSwitch var cs ds =
    SStatement $ SWITCH LEFTPAREN var RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
    Nothing
    (Just $ fromList $ concat [(LStatement $ CASE l COLON $ frst_stmt ss) : rest_stmt ss | (l, ss) <- cs]
                              ++ (LStatement $ DEFAULT COLON $ frst_stmt ds) : rest_stmt ds)
    RIGHTCURLY
    where
        estmt e = EStatement $ ExpressionStatement (Just e) SEMICOLON
        frst_stmt (e:_) = estmt e
        frst_stmt []    = JStatement $ BREAK SEMICOLON
        rest_stmt (_:es) = map estmt es ++ [JStatement $ BREAK SEMICOLON]
        rest_stmt []     = []

makeEnum :: Identifier -> [Identifier] -> TypeSpecifier
makeEnum x [] = ENUM (Left $ x)
makeEnum x cs = 
    ENUM (Right (Quad (if null x then Nothing else Just $ x)
    LEFTCURLY
    (fromList [Enumerator c Nothing | c <- cs])
    RIGHTCURLY))

makeStruct :: Identifier -> [(SpecifierQualifierList, Identifier)] -> TypeSpecifier
makeStruct name [] = STRUCT (Left $ name)
makeStruct name ss = 
    STRUCT (Right (Quad (Just $ name)
    LEFTCURLY
    (fromList [StructDeclaration sqs (fromList [StructDeclarator $ This $ Declarator Nothing $ IDirectDeclarator id]) SEMICOLON | (sqs, id) <- ss])
    RIGHTCURLY))

(<*$>) :: Applicative f => f (a -> b) -> a -> f b
f <*$> a = f <*> pure a
infixl 4 <*$>

(<*.>) :: Functor f => f (b -> c) -> (a -> b) -> f (a -> c)
f <*.> a = (. a) <$> f
infixl 4 <*.>

pop :: Monad m => StateT [a] m a
pop = state $ head &&& tail

convertIR :: Alias QualifiedName -> Bool -> SmudgeIR -> [ExternalDeclaration]
convertIR aliases dodef ir = map (ExternalDeclaration . Right . convertDef) ir ++
                             if dodef then concatMap (map (ExternalDeclaration . Left) . defineDef) ir else []
    where
        defineDef (FunDef name ps (b, ty) ds es) = [defun (first (bind b) $ convertNamedDeclarator ps name ty) $ convertBlock ds es]
        defineDef (DecDef _)                     = []

        convertDef (FunDef name ps (b, ty) ds es) = define (first (bind b) $ convertDeclarator name ty) Nothing
        convertDef (DecDef d)                     = convertDec d

        convertDec (TyDec x ty)           = define (typedef x $ convertStruct ty) Nothing
        convertDec (CaseDec x cs)         = define (typedef x $ convertEnum x cs) Nothing
        convertDec (VarDec x (b, ty) e)   = define (first (bind b) $ convertDeclarator x ty) (Just $ AInitializer $ convertExpr e)
        convertDec (ListDec x (b, ty) es) = define (first (bind b) $ convertDeclaratorList) (Just $ LInitializer LEFTCURLY (fromList $ map (AInitializer . convertExpr) es) Nothing RIGHTCURLY)
            where convertDeclaratorList = second (convertFromAbsDeclarator x . Just . addList . theseAndThat Nothing id . sequence) $ convertAbsDeclarator ty
                  addList = fmapThis (const $ fromList [POINTER $ Just $ fromList [CONST]]) . fmap (\a -> CDirectAbstractDeclarator a LEFTSQUARE Nothing RIGHTSQUARE)
        convertDec (SizeDec x y)          = define (fromList [A STATIC, C CONST, B UNSIGNED, B INT], (Declarator Nothing (IDirectDeclarator $ convertQName x)))
                                                        (Just $ AInitializer count_e)
            where a_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier $ convertQName y]) Nothing) RIGHTPAREN) (:#)
                  ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                               (Just $ This $ fromList [POINTER Nothing])) RIGHTPAREN) (:#)
                  count_e = (#:) (a_size_e `DIV` ptr_size_e) (:#)

        bind :: Binding -> DeclarationSpecifiers -> DeclarationSpecifiers
        bind External   ds = A EXTERN <: ds
        bind Unresolved ds = ds
        bind Exported   ds = ds
        bind Internal   ds = A STATIC <: ds

        typedef :: TaggedName -> DeclarationSpecifiers -> (DeclarationSpecifiers, Declarator)
        typedef x ds = (A TYPEDEF <: ds, Declarator Nothing (IDirectDeclarator $ convertTName x))

        defun :: (DeclarationSpecifiers, Declarator) -> CompoundStatement -> FunctionDefinition
        defun (spec, declr) body = Function (Just spec) declr Nothing body

        define :: (DeclarationSpecifiers, Declarator) -> Maybe Initializer  -> Declaration
        define (spec, declr) init = Declaration spec (Just $ fromList [InitDeclarator declr initializer]) SEMICOLON
            where initializer = fmap (Pair EQUAL) init

        convertEnum :: TaggedName -> [QualifiedName] -> DeclarationSpecifiers
        convertEnum x cs = fromList [B $ makeEnum (convertTName x) (map convertQName cs)]

        convertStruct :: TaggedName -> DeclarationSpecifiers
        convertStruct x = fromList [B $ makeStruct (convertTName x) []]

        constable (TagEvent _)   = True
        constable (TagBuiltin _) = True
        constable _              = False

        convertAbsDeclarator :: Ty -> (DeclarationSpecifiers, Maybe AbstractDeclarator)
        convertAbsDeclarator Void                 = (fromList [B VOID], Nothing)
        convertAbsDeclarator (Ty t) | constable t = (fromList [C CONST, B $ TypeSpecifier $ convertTName t], Just $ This $ fromList [POINTER Nothing])
        convertAbsDeclarator (Ty t)               = (fromList [B $ TypeSpecifier $ convertTName t], Nothing)
        convertAbsDeclarator (p :-> r)            = (spec, Just absdec)
            where ((spec, rabsdec), rparams) = convertF (p :-> r)
                  params = ParameterTypeList (fromList rparams) Nothing
                  pdeclr dad = PDirectAbstractDeclarator dad LEFTPAREN (Just params) RIGHTPAREN
                  absdec = fmap pdeclr $ theseAndThat Nothing id $ sequence rabsdec
                  toParam = uncurry ParameterDeclaration . second (fmap Right) . convertAbsDeclarator
                  convertF (p :-> r) = second (toParam p :) $ convertF r
                  convertF        r  = (convertAbsDeclarator r, [])

        convertDeclarator :: QualifiedName -> Ty -> (DeclarationSpecifiers, Declarator)
        convertDeclarator x = second (convertFromAbsDeclarator x) . convertAbsDeclarator

        convertFromAbsDeclarator :: QualifiedName -> Maybe AbstractDeclarator -> Declarator
        convertFromAbsDeclarator x = inst
            where maybename = maybe (IDirectDeclarator $ convertQName x) instdir
                  inst ad = Declarator (ad >>= maybeThis) (maybename $ ad >>= maybeThat)
                  instdir (ADirectAbstractDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN (inst $ Just a) RIGHTPAREN
                  instdir (CDirectAbstractDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator (maybename a) LEFTSQUARE c RIGHTSQUARE
                  instdir (PDirectAbstractDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator (maybename a) LEFTPAREN (fmap Left p) RIGHTPAREN

        convertNamedDeclarator :: [QualifiedName] -> QualifiedName -> Ty -> (DeclarationSpecifiers, Declarator)
        convertNamedDeclarator names = (second (flip evalState names . namedr) .) . convertDeclarator
            where namedr (Declarator ps dd) = Declarator ps <$> namedd dd
                  namedd (IDirectDeclarator i)                          = return $ IDirectDeclarator i
                  namedd (DDirectDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN <$> namedr a <*$> RIGHTPAREN
                  namedd (CDirectDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator <$> namedd a <*$> LEFTSQUARE <*$> c <*$> RIGHTSQUARE
                  namedd (PDirectDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator <$> namedd a <*$> LEFTPAREN <*> mapM (either ((Left <$>) . nameptl) (return . Right)) p <*$> RIGHTPAREN
                  nameptl (ParameterTypeList pl ell) = ParameterTypeList <$> namepl pl <*$> ell
                  namepl (CommaList p ps) = CommaList <$> namep p <*> mapM (\(Pair COMMA p) -> Pair COMMA <$> namepl p) ps
                  namep (ParameterDeclaration spec@(SimpleList (B VOID) _) declr) = ParameterDeclaration spec <$> fmap Left <$> mapM (either id <$> (convertFromAbsDeclarator <$> pop <*.> Just) <*$>) declr
                  namep (ParameterDeclaration spec declr) = ParameterDeclaration spec <$> Just <$> Left <$> maybe (Declarator Nothing . IDirectDeclarator . convertQName) (either const (flip convertFromAbsDeclarator . Just)) declr <$> pop

        convertBlock ds ss = CompoundStatement LEFTCURLY
                                (if null ds then Nothing else Just $ fromList $ map convertDec ds)
                                (if null ss then Nothing else Just $ fromList $ map convertStmt ss)
                             RIGHTCURLY

        convertStmt (Cases e cs ds) = makeSwitch (fromList [convertExpr e]) (map (convertToConstExpr *** map convertToExpression) cs) (map convertToExpression ds)
        convertStmt (If e ss)       = SStatement $ IF LEFTPAREN (fromList [convertExpr e]) RIGHTPAREN (CStatement $ convertBlock [] ss) Nothing
        convertStmt (Return e)      = JStatement $ RETURN (Just $ fromList [convertExpr e]) SEMICOLON
        convertStmt (ExprS e)       = EStatement $ ExpressionStatement (Just $ fromList [convertExpr e]) SEMICOLON

        convertToConstExpr q = (#:) (convertQName q) (:#)
        convertToExpression = fromList . (:[]) . convertExpr

        convertExpr (FunCall x es)      = (#:) (apply ((#:) (convertQName x) (:#)) (map convertExpr es)) (:#)
        convertExpr (Literal (Strn v))  = (#:) (show v) (:#)
        convertExpr (Literal (Nmbr v))  = (#:) (show v) (:#)
        convertExpr (Value x)           = (#:) (convertQName x) (:#)
        convertExpr (Assign x e)        = (#:) (convertQName x) (:#) `ASSIGN` convertExpr e
        convertExpr (Neq x1 x2)         = (#:) (((#:) (convertQName x1) (:#)) `NOTEQUAL` ((#:) (convertQName x2) (:#))) (:#)
        convertExpr (SafeIndex a i b d) = (#:) (bounds_check_e `QUESTION` (Trio (fromList [array_index_e]) COLON ((#:) (show d) (:#)))) (:#)
            where bounds_check_e = (#:) (((#:) (convertQName i) (:#)) `LESS_THAN` ((#:) (convertQName b) (:#))) (:#)
                  array_index_e = (#:) (EPostfixExpression ((#:) (convertQName a) (:#)) LEFTSQUARE (fromList [(#:) (convertQName i) (:#)]) RIGHTSQUARE) (:#)

        convertQName :: Qualifiable q => q -> Identifier
        convertQName q = mangleWith (+-+) mangleIdentifier $ rename aliases $ qualify q

        convertTName :: TaggedName -> Identifier
        convertTName (TagEvent q) = convertQName q +-+ "t"
        convertTName t = convertQName t

        apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
        apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
        apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

instance Backend CStaticOption where
    options = ("c",
               [Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["h"] (ReqArg Header "FILE")
                 "The name of the target header file if not derived from source file.",
                Option [] ["ext_h"] (ReqArg ExtFile "FILE")
                 "The name of the target ext header file if not derived from source file.",
                Option [] ["no-debug"] (NoArg NoDebug)
                 "Don't generate debugging information"])
    generate os gswust outputTarget = sequence $ [writeTranslationUnit (renderHdr hdr []) (headerName os),
                                         writeTranslationUnit (renderSrc src [extHdrName os, headerName os]) (outputName os)]
                                         ++ [writeTranslationUnit (renderHdr ext [headerName os]) (extHdrName os) | not $ null tue]
        where src = fromList tus
              ext = fromList tue
              hdr = fromList tuh
              tus = concat [convertIR aliases True (lower debug ([g], syms)) | g <- gs]
              tue = convertIR aliases False $ lowerSymTab $ filterBind syms External
              tuh = convertIR aliases False $ (lowerSymTab $ filterBind syms Exported) ++ (lowerSymTab externs)
              (gs, aliases, syms) = gswust
              externs = insertFunctions mempty [(rename aliases $ qualify (smName, "Current_state_name"), ([], "char")) | (Annotated _ (StateMachineDeclarator smName), _) <- gs]
              inc ^++ src = (liftM (++src)) inc
              writeTranslationUnit render fp = (render fp) >>= (writeFile fp) >> (return fp)
              renderHdr u includes fp = hdrLeader includes fp ^++ (renderPretty u ++ hdrTrailer)
              renderSrc u includes fp = srcLeader includes fp ^++ (renderPretty u ++ srcTrailer)
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputFileName ((OutFile f):_) = f
              outputFileName xs = getFirstOrDefault outputFileName ((dropExtension outputTarget) <.> "c") xs
              outputName = normalise . outputFileName
              headerFileName ((Header f):_) = f
              headerFileName xs = getFirstOrDefault headerFileName ((dropExtension outputTarget) <.> "h") xs
              headerName = normalise . headerFileName
              extHdrFileName ((ExtFile f):_) = f
              extHdrFileName xs = getFirstOrDefault extHdrFileName (((dropExtension outputTarget) ++ "_ext") <.> "h") xs
              extHdrName = normalise . extHdrFileName
              doDebug ((NoDebug):_) = False
              doDebug xs = getFirstOrDefault doDebug True xs
              genIncludes includes includer = liftM concat $ sequence $ map (mkInclude includer) includes
              mkInclude includer include =
                do
                  relativeInclude <- relPath (takeDirectory includer) include
                  return $ concat ["#include \"", relativeInclude, "\"\n"]
              srcLeader = genIncludes
              srcTrailer = ""
              reinclusionName fp = concat ["__", map (\a -> (if a == '.' then '_' else a)) (takeFileName fp), "__"]
              hdrLeader includes fp =
                do
                  gennedIncludes <- genIncludes includes fp
                  return $ concat ["#ifndef ", reinclusionName fp, "\n", "#define ", reinclusionName fp, "\n",
                                   gennedIncludes]
              hdrTrailer = "#endif\n"
              debug = doDebug os
