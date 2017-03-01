{-# LANGUAGE FlexibleContexts #-}

module Backends.CStatic (
    CStaticOption(..)
) where

import Backends.Backend (Backend(..))
import Backends.SmudgeIR (
  SmudgeIR,
  Def(..),
  DataDef(..),
  TyDec(..),
  Init(..),
  VarDec(..),
  Stmt(..),
  Expr(..),
  lower,
  lowerSymTab,
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

data FileType = Source | Header
    deriving (Show, Eq)

data FileCategory = ExtFile | IntFile
    deriving (Show, Eq)

data CStaticOption = TargetPath FileType FileCategory FilePath
                   | NoDebug
                   | GenStubs
    deriving (Show, Eq)

(+-+) :: Identifier -> Identifier -> Identifier
a +-+ b = dropWhileEnd (== '_') a ++ "_" ++ dropWhile (== '_') b

makeSwitch :: Expression -> [(ConstantExpression, [Statement])] -> [Statement] -> Statement
makeSwitch var cs ds =
    SStatement $ SWITCH LEFTPAREN var RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
    Nothing
    (Just $ fromList $ concat [(LStatement $ CASE l COLON $ frst_stmt ss) : rest_stmt ss | (l, ss) <- cs]
                              ++ (LStatement $ DEFAULT COLON $ frst_stmt ds) : rest_stmt ds)
    RIGHTCURLY
    where
        frst_stmt (e:_) = e
        frst_stmt []    = JStatement $ BREAK SEMICOLON
        rest_stmt (_:es) = es ++ [JStatement $ BREAK SEMICOLON]
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
    (fromList [StructDeclaration sqs (fromList [This $ Declarator Nothing $ IDirectDeclarator id]) SEMICOLON | (sqs, id) <- ss])
    RIGHTCURLY))

(<*$>) :: Applicative f => f (a -> b) -> a -> f b
f <*$> a = f <*> pure a
infixl 4 <*$>

(<*.>) :: Functor f => f (b -> c) -> (a -> b) -> f (a -> c)
f <*.> a = (. a) <$> f
infixl 4 <*.>

pop :: Monad m => StateT [a] m a
pop = state $ head &&& tail

convertIR :: Alias QualifiedName -> Bool -> Bool -> SmudgeIR -> [ExternalDeclaration]
convertIR aliases dodec dodef ir =
            (if dodec then map (ExternalDeclaration . Right . convertDef) ir else []) ++
            (if dodef then concatMap (map (ExternalDeclaration . Left) . defineDef) ir else [])
    where
        defineDef (FunDef name ps (b, ty) ds es) = [defun (first (bind b) $ convertNamedDeclarator ps name ty) $ convertBlock ds es]
        defineDef (DataDef _)                    = []

        convertDef :: Def -> Declaration
        convertDef (FunDef name ps (b, ty) ds es) = define (first (bind b) $ convertDeclarator name ty) Nothing
        convertDef (DataDef d)                    = convertDec d

        convertDec :: DataDef -> Declaration
        convertDec (TyDef d)    = define (convertTyDec d) Nothing
        convertDec (VarDef b d) = uncurry define $ first (first (bind b)) $ second Just $ convertVarDef d

        convertTyDec :: TyDec -> (DeclarationSpecifiers, Declarator)
        convertTyDec (EvtDec x ty)  = typedef x $ convertStruct ty
        convertTyDec (CaseDec x cs) = typedef x $ convertEnum x cs

        convertVarDef :: VarDec Init -> ((SpecifierQualifierList, Declarator), Initializer)
        convertVarDef v@(ValDec _ _ (Init e))   = (convertVarDec v, AInitializer $ convertExpr e)
        convertVarDef v@(ListDec _ _ (Init es)) = (convertVarDec v, LInitializer LEFTCURLY (fromList $ map (AInitializer . convertExpr) es) Nothing RIGHTCURLY)
        convertVarDef v@(SizeDec _ (Init y))    = (convertVarDec v, AInitializer count_e)
            where a_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier $ convertQName y]) Nothing) RIGHTPAREN) (:#)
                  ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                               (Just $ This $ fromList [POINTER Nothing])) RIGHTPAREN) (:#)
                  count_e = (#:) (a_size_e `DIV` ptr_size_e) (:#)

        convertVarDec :: VarDec a -> (SpecifierQualifierList, Declarator)
        convertVarDec (ValDec x ty _)  = convertDeclarator x ty
        convertVarDec (ListDec x ty _) = second (convertFromAbsDeclarator x . Just . addList . theseAndThat Nothing id . sequence) $ convertAbsDeclarator ty
            where addList = fmapThis (const $ fromList [POINTER $ Just $ fromList [CONST]]) . fmap (\a -> CDirectAbstractDeclarator a LEFTSQUARE Nothing RIGHTSQUARE)
        convertVarDec (SizeDec x _)    = (fromList [Right CONST, Left UNSIGNED, Left INT], (Declarator Nothing (IDirectDeclarator $ convertQName x)))

        sqlToDss :: SpecifierQualifierList -> DeclarationSpecifiers
        sqlToDss (SimpleList sq xs) = SimpleList (sqToDs sq) $ fmap sqlToDss xs
            where sqToDs (Left ts)  = B ts
                  sqToDs (Right tq) = C tq

        bind :: Binding -> SpecifierQualifierList -> DeclarationSpecifiers
        bind b sql = bind' b $ sqlToDss sql
            where bind' External   ds = A EXTERN <: ds
                  bind' Unresolved ds = ds
                  bind' Exported   ds = ds
                  bind' Internal   ds = A STATIC <: ds

        typedef :: TaggedName -> SpecifierQualifierList -> (DeclarationSpecifiers, Declarator)
        typedef x ds = (A TYPEDEF <: sqlToDss ds, Declarator Nothing (IDirectDeclarator $ convertTName x))

        defun :: (DeclarationSpecifiers, Declarator) -> CompoundStatement -> FunctionDefinition
        defun (spec, declr) body = Function (Just spec) declr Nothing body

        define :: (DeclarationSpecifiers, Declarator) -> Maybe Initializer  -> Declaration
        define (spec, declr) init = Declaration spec (Just $ fromList [InitDeclarator declr initializer]) SEMICOLON
            where initializer = fmap (Pair EQUAL) init

        convertEnum :: TaggedName -> [QualifiedName] -> SpecifierQualifierList
        convertEnum x cs = fromList [Left $ makeEnum (convertTName x) (map convertQName cs)]

        convertStruct :: TaggedName -> SpecifierQualifierList
        convertStruct x = fromList [Left $ makeStruct (convertTName x) []]

        constable (TagEvent _)   = True
        constable (TagBuiltin _) = True
        constable _              = False

        convertAbsDeclarator :: Ty -> (SpecifierQualifierList, Maybe AbstractDeclarator)
        convertAbsDeclarator Void                 = (fromList [Left VOID], Nothing)
        convertAbsDeclarator (Ty t) | constable t = (fromList [Right CONST, Left $ TypeSpecifier $ convertTName t], Just $ This $ fromList [POINTER Nothing])
        convertAbsDeclarator (Ty t)               = (fromList [Left $ TypeSpecifier $ convertTName t], Nothing)
        convertAbsDeclarator (p :-> r)            = (spec, Just absdec)
            where ((spec, rabsdec), rparams) = convertF (p :-> r)
                  params = ParameterTypeList (fromList rparams) Nothing
                  pdeclr dad = PDirectAbstractDeclarator dad LEFTPAREN (Just params) RIGHTPAREN
                  absdec = fmap pdeclr $ theseAndThat Nothing id $ sequence rabsdec
                  toParam = uncurry ParameterDeclaration . second (fmap Right) . first sqlToDss . convertAbsDeclarator
                  convertF (p :-> r) = second (toParam p :) $ convertF r
                  convertF        r  = (convertAbsDeclarator r, [])

        convertDeclarator :: QualifiedName -> Ty -> (SpecifierQualifierList, Declarator)
        convertDeclarator x = second (convertFromAbsDeclarator x) . convertAbsDeclarator

        convertFromAbsDeclarator :: QualifiedName -> Maybe AbstractDeclarator -> Declarator
        convertFromAbsDeclarator x = inst
            where maybename = maybe (IDirectDeclarator $ convertQName x) instdir
                  inst ad = Declarator (ad >>= maybeThis) (maybename $ ad >>= maybeThat)
                  instdir (ADirectAbstractDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN (inst $ Just a) RIGHTPAREN
                  instdir (CDirectAbstractDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator (maybename a) LEFTSQUARE c RIGHTSQUARE
                  instdir (PDirectAbstractDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator (maybename a) LEFTPAREN (fmap Left p) RIGHTPAREN

        convertNamedDeclarator :: [QualifiedName] -> QualifiedName -> Ty -> (SpecifierQualifierList, Declarator)
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

        convertStmt (Cases e cs ds) = makeSwitch (fromList [convertExpr e]) (map (convertToConstExpr *** map convertStmt) cs) (map convertStmt ds)
        convertStmt (If e ss)       = SStatement $ IF LEFTPAREN (fromList [convertExpr e]) RIGHTPAREN (CStatement $ convertBlock [] ss) Nothing
        convertStmt (Return e)      = JStatement $ RETURN (Just $ fromList [convertExpr e]) SEMICOLON
        convertStmt (ExprS e)       = EStatement $ ExpressionStatement (Just $ fromList [convertExpr e]) SEMICOLON

        convertToConstExpr q = (#:) (convertQName q) (:#)

        convertExpr (FunCall x es)      = (#:) (apply ((#:) (convertQName x) (:#)) (map convertExpr es)) (:#)
        convertExpr (Literal v)         = (#:) (show v) (:#)
        convertExpr (Null)              = (#:) "0" (:#)
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
               [Option [] ["o"] (ReqArg (TargetPath Source IntFile) "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["h"] (ReqArg (TargetPath Header IntFile) "FILE")
                 "The name of the target header file if not derived from source file.",
                Option [] ["ext_o"] (ReqArg (TargetPath Source ExtFile) "FILE")
                 "The name of the target ext file if not derived from source file.",
                Option [] ["ext_h"] (ReqArg (TargetPath Header ExtFile) "FILE")
                 "The name of the target ext header file if not derived from source file.",
                Option [] ["stubs"] (NoArg GenStubs)
                 "generate stub implementation.",
                Option [] ["no-debug"] (NoArg NoDebug)
                 "Don't generate debugging information"])
    generate os gswust outputTarget = sequence $ [writeTranslationUnit (renderHdr hdr []) headerName,
                                         writeTranslationUnit (renderSrc src [extHdrName, headerName]) outputName,
                                         writeTranslationUnit (renderHdr ext [headerName]) extHdrName] ++
                                         if stubs then [writeTranslationUnit (renderSrc exs [extHdrName, headerName]) extSrcName] else []
        where hdr = fromList $ convertIR aliases True False $ lowerSymTab $ filterBind syms Exported
              src = fromList $ concat [convertIR aliases True True (lower debug ([g], syms)) | g <- gs]
              ext = fromList $ convertIR aliases True False $ lowerSymTab $ filterBind syms External
              exs = fromList $ convertIR aliases False True $ lowerSymTab $ filterBind syms External
              (gs, aliases, syms) = gswust
              inc ^++ src = (liftM (++src)) inc
              writeTranslationUnit render fp = (render fp) >>= (writeFile fp) >> (return fp)
              renderHdr u includes fp = hdrLeader includes fp ^++ (renderPretty u ++ hdrTrailer)
              renderSrc u includes fp = srcLeader includes fp ^++ (renderPretty u ++ srcTrailer)

              outputBaseName = dropExtension outputTarget
              outputExtName = outputBaseName ++ "_ext"
              renames = [o | o@(TargetPath _ _ _) <- os]
              headerName = normalise $ head $ [f | TargetPath Header IntFile f <- renames] ++ [(outputBaseName <.> "h")]
              outputName = normalise $ head $ [f | TargetPath Source IntFile f <- renames] ++ [(outputBaseName <.> "c")]
              extHdrName = normalise $ head $ [f | TargetPath Header ExtFile f <- renames] ++ [(outputExtName <.> "h")]
              extSrcName = normalise $ head $ [f | TargetPath Source ExtFile f <- renames] ++ [(outputExtName <.> "c")]
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
              debug = null $ filter (==NoDebug) os
              stubs = not $ null $ filter (==GenStubs) os
