-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Backends.CStatic (
    CStaticOption(..)
) where

import Language.Smudge.Backends.Backend (
  Backend(..),
  Config(..)
  )
import Language.Smudge.Backends.SmudgeIR (
  SmudgeIR,
  Ty(..),
  Def(..),
  DataDef(..),
  TyDec(..),
  Init(..),
  VarDec(..),
  Stmt(..),
  Expr(..),
  Var(..),
  lower,
  lowerSymTab,
  )
import Language.C89.Grammar
import Language.Smudge.Semantics.Model (
  QualifiedName,
  Qualifiable(qualify),
  extractWith,
  TaggedName,
  Tagged(..),
  )
import Language.Smudge.Passes.Passes (
  Fault(..),
  Severity(..),
  )
import Language.Smudge.Semantics.Solver (
  Binding(..),
  filterBind,
  (!),
  )
import Language.Smudge.Parsers.Id (
  rawtest,
  mangle,
  )
import Language.Smudge.Semantics.Alias (
    Alias,
    rename,
    traverseAliases,
    )
import System.Console.GetOpt.Extra (OptDescr(..), ArgDescr(..))
import System.FilePath.Extra (relPath)
import Data.These (
  These(..),
  maybeThis,
  maybeThat,
  fmapThis,
  theseAndThat,
  )
import Language.C89.Unparser (renderPretty)

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative (liftA)
import Control.Monad.State (StateT, evalState, state)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (Except, mapExceptT, throwE)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, isNothing)
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
                   | GenStubs
    deriving (Show, Eq)

(+-+) :: Identifier -> Identifier -> Identifier
a +-+ b = a ++ "_" ++ b
infixr 5 +-+

a <++ b = (liftA (++ b)) a
infixr 5 <++

makeSwitch :: Expression -> [(ConstantExpression, [Statement])] -> [Statement] -> Statement
makeSwitch var cs ds =
    SStatement $ SWITCH LEFTPAREN var RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
    Nothing
    (Just $ fromList $ concat [(LStatement $ CASE l COLON $ head ss') : tail ss' | (l, ss) <- cs, let ss' = withBreak ss]
                              ++ (LStatement $ DEFAULT COLON $ head ds') : tail ds')
    RIGHTCURLY
    where
        withBreak = (++ [JStatement $ BREAK SEMICOLON])
        ds' = withBreak ds

makeEnum :: Identifier -> [Identifier] -> TypeSpecifier
makeEnum x [] = ENUM (Left $ x)
makeEnum x cs =
    ENUM (Right (Quad (if null x then Nothing else Just $ x)
    LEFTCURLY
    (fromList [Enumerator c Nothing | c <- cs])
    RIGHTCURLY))

makeStruct :: Identifier -> [(SpecifierQualifierList, Declarator)] -> TypeSpecifier
makeStruct = makeStructOrUnion STRUCT

makeUnion :: Identifier -> [(SpecifierQualifierList, Declarator)] -> TypeSpecifier
makeUnion = makeStructOrUnion UNION

makeStructOrUnion ctor x [] = ctor (Left $ x)
makeStructOrUnion ctor x ss =
    ctor (Right (Quad (if null x then Nothing else Just $ x)
    LEFTCURLY
    (fromList [StructDeclaration sqs (fromList [This declr]) SEMICOLON | (sqs, declr) <- ss])
    RIGHTCURLY))

(<*$>) :: Applicative f => f (a -> b) -> a -> f b
f <*$> a = f <*> pure a
infixl 4 <*$>

(<*.>) :: Functor f => f (b -> c) -> (a -> b) -> f (a -> c)
f <*.> a = (. a) <$> f
infixl 4 <*.>

pop :: Monad m => StateT [a] m a
pop = state $ head &&& tail

data FullyQualAndEvent x = FullyQualAndEvent { fullQual :: x, evtQual :: x }

convertIR :: Bool -> Bool -> (Alias Identifier, SmudgeIR (FullyQualAndEvent Identifier)) -> [ExternalDeclaration]
convertIR dodec dodef (aliases, ir) =
            (if dodec then map (ExternalDeclaration . Right . convertDef) ir else []) ++
            (if dodef then concatMap (map (ExternalDeclaration . Left) . defineDef) ir else [])
    where
        defineDef :: Def (FullyQualAndEvent Identifier) -> [FunctionDefinition]
        defineDef (FunDef name ps (b, ty) ds es) = [defun (first (bind b) $ convertNamedDeclarator (map fullQual ps) (fullQual name) (fmap fullQual ty)) $ convertBlock ds es]
        defineDef (DataDef _)                    = []

        convertDef :: Def (FullyQualAndEvent Identifier) -> Declaration
        convertDef (FunDef name ps (b, ty) ds es) = define (first (bind b) $ convertDeclarator (fullQual name) (fmap fullQual ty)) Nothing
        convertDef (DataDef d)                    = convertDec d

        convertDec :: DataDef (FullyQualAndEvent Identifier) -> Declaration
        convertDec (TyDef x d)  = define (typedef x $ convertTyDec d) Nothing
        convertDec (VarDef b d) = uncurry define $ first (first (bind b)) $ convertVarDef d

        convertTyDec :: TyDec (FullyQualAndEvent Identifier) -> SpecifierQualifierList
        convertTyDec (EvtDec ty)   = convertStruct ty
        convertTyDec (SumDec x cs) = if all isNothing cvs then convertEnum x cxs else taggedUnion
            where (cxs, cvs) = unzip cs
                  tag = fromList [Left $ makeEnum "" $ map fullQual cxs]
                  union = fromList [Left $ makeUnion "" $ map (uncurry convertDeclarator . (foldMap evtQual &&& Ty . fmap fullQual)) $ catMaybes cvs]
                  taggedUnion = fromList [Left $ makeStruct (convertTName $ fmap fullQual x) [(tag, tag_name), (union, union_name)]]
                  tag_name = Declarator Nothing $ IDirectDeclarator id_field
                  union_name = Declarator Nothing $ IDirectDeclarator event_field

        convertVarDef :: VarDec Init (FullyQualAndEvent Identifier) -> ((SpecifierQualifierList, Declarator), Maybe Initializer)
        convertVarDef v@(ValDec _ _ (Init e))   = (convertVarDec v, Just $ AInitializer $ convertExpr e)
        convertVarDef v@(SumVDec _ _ _)         = (convertVarDec v, Nothing)
        convertVarDef v@(ListDec _ _ (Init es)) = (convertVarDec v, Just $ LInitializer LEFTCURLY (fromList $ map (AInitializer . convertExpr) es) Nothing RIGHTCURLY)
        convertVarDef v@(SizeDec _ (Init y))    = (convertVarDec v, Just $ AInitializer count_e)
            where a_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier $ fullQual y]) Nothing) RIGHTPAREN) (:#)
                  ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                               (Just $ This $ fromList [POINTER Nothing])) RIGHTPAREN) (:#)
                  count_e = (#:) (a_size_e `DIV` ptr_size_e) (:#)

        convertVarDec :: VarDec a (FullyQualAndEvent Identifier) -> (SpecifierQualifierList, Declarator)
        convertVarDec (ValDec x ty _)  = convertDeclarator (fullQual x) (fmap fullQual ty)
        convertVarDec (SumVDec x ty _) = convertDeclarator (fullQual x) (fmap fullQual ty)
        convertVarDec (ListDec x ty _) = second (convertFromAbsDeclarator (fullQual x) . Just . addList . theseAndThat Nothing id . sequence) $ convertAbsDeclarator (fmap fullQual ty)
            where addList = fmapThis (const $ fromList [POINTER $ Just $ fromList [CONST]]) . fmap (\a -> CDirectAbstractDeclarator a LEFTSQUARE Nothing RIGHTSQUARE)
        convertVarDec (SizeDec x _)    = (fromList [Right CONST, Left UNSIGNED, Left INT], (Declarator Nothing $ IDirectDeclarator $ fullQual x))

        id_field = rename aliases "id"
        event_field = rename aliases "event"

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

        typedef :: Tagged (FullyQualAndEvent Identifier) -> SpecifierQualifierList -> (DeclarationSpecifiers, Declarator)
        typedef x ds = (A TYPEDEF <: sqlToDss ds, Declarator Nothing (IDirectDeclarator $ convertTName $ fmap fullQual x))

        defun :: (DeclarationSpecifiers, Declarator) -> CompoundStatement -> FunctionDefinition
        defun (spec, declr) body = Function (Just spec) declr Nothing body

        define :: (DeclarationSpecifiers, Declarator) -> Maybe Initializer  -> Declaration
        define (spec, declr) init = Declaration spec (Just $ fromList [InitDeclarator declr initializer]) SEMICOLON
            where initializer = fmap (Pair EQUAL) init

        convertEnum :: Tagged (FullyQualAndEvent Identifier) -> [FullyQualAndEvent Identifier] -> SpecifierQualifierList
        convertEnum x cs = fromList [Left $ makeEnum (convertTName $ fmap fullQual x) $ map fullQual cs]

        convertStruct :: Tagged (FullyQualAndEvent Identifier) -> SpecifierQualifierList
        convertStruct x = fromList [Left $ makeStruct (convertTName $ fmap fullQual x) []]

        constable (TagEvent _)   = True
        constable (TagBuiltin _) = True
        constable _              = False

        convertAbsDeclarator :: Ty Identifier -> (SpecifierQualifierList, Maybe AbstractDeclarator)
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

        convertDeclarator :: Identifier -> Ty Identifier -> (SpecifierQualifierList, Declarator)
        convertDeclarator x = second (convertFromAbsDeclarator x) . convertAbsDeclarator

        convertFromAbsDeclarator :: Identifier -> Maybe AbstractDeclarator -> Declarator
        convertFromAbsDeclarator x = inst
            where maybename = maybe (IDirectDeclarator x) instdir
                  inst ad = Declarator (ad >>= maybeThis) (maybename $ ad >>= maybeThat)
                  instdir (ADirectAbstractDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN (inst $ Just a) RIGHTPAREN
                  instdir (CDirectAbstractDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator (maybename a) LEFTSQUARE c RIGHTSQUARE
                  instdir (PDirectAbstractDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator (maybename a) LEFTPAREN (fmap Left p) RIGHTPAREN

        convertNamedDeclarator :: [Identifier] -> Identifier -> Ty Identifier ->  (SpecifierQualifierList, Declarator)
        convertNamedDeclarator names = (second (flip evalState names . namedr) .) . convertDeclarator
            where namedr (Declarator ps dd) = Declarator ps <$> namedd dd
                  namedd (IDirectDeclarator i)                          = return $ IDirectDeclarator i
                  namedd (DDirectDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN <$> namedr a <*$> RIGHTPAREN
                  namedd (CDirectDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator <$> namedd a <*$> LEFTSQUARE <*$> c <*$> RIGHTSQUARE
                  namedd (PDirectDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator <$> namedd a <*$> LEFTPAREN <*> mapM (either ((Left <$>) . nameptl) (return . Right)) p <*$> RIGHTPAREN
                  nameptl (ParameterTypeList pl ell) = ParameterTypeList <$> namepl pl <*$> ell
                  namepl (CommaList p ps) = CommaList <$> namep p <*> mapM (\(Pair COMMA p) -> Pair COMMA <$> namepl p) ps
                  namep (ParameterDeclaration spec@(SimpleList (B VOID) _) declr) = ParameterDeclaration spec <$> fmap Left <$> mapM (either id <$> (convertFromAbsDeclarator <$> pop <*.> Just) <*$>) declr
                  namep (ParameterDeclaration spec declr) = ParameterDeclaration spec <$> Just <$> Left <$> maybe (Declarator Nothing . IDirectDeclarator) (either const (flip convertFromAbsDeclarator . Just)) declr <$> pop

        convertBlock :: [DataDef (FullyQualAndEvent Identifier)] -> [Stmt (FullyQualAndEvent Identifier)] -> CompoundStatement
        convertBlock ds ss = CompoundStatement LEFTCURLY
                                (if null ds then Nothing else Just $ fromList $ map convertDec ds)
                                (if null ss then Nothing else Just $ fromList $ map convertStmt $ sum_inits ++ ss)
                             RIGHTCURLY
            where sum_inits = concat [[ExprS $ Assign v (Value $ Var ctor), ExprS $ Assign (Field v ctor) e]
                                      | VarDef _ (SumVDec x _ (Init (ctor, e))) <- ds, let v = SumVar x]

        convertStmt :: Stmt (FullyQualAndEvent Identifier) -> Statement
        convertStmt (Cases e cs ds) = makeSwitch (fromList [convertExpr e]) (map (convertToConstExpr *** map convertStmt) cs) (map convertStmt ds)
        convertStmt (If e ss)       = SStatement $ IF LEFTPAREN (fromList [convertExpr e]) RIGHTPAREN (CStatement $ convertBlock [] ss) Nothing
        convertStmt (Return e)      = JStatement $ RETURN (Just $ fromList [convertExpr e]) SEMICOLON
        convertStmt (ExprS e)       = EStatement $ ExpressionStatement (Just $ fromList [convertExpr e]) SEMICOLON

        convertToConstExpr q = (#:) (fullQual q) (:#)

        convertExpr :: Expr (FullyQualAndEvent Identifier) -> AssignmentExpression
        convertExpr (FunCall x es)      = (#:) (apply ((#:) (fullQual x) (:#)) (map convertExpr es)) (:#)
        convertExpr (Literal v)         = (#:) (show v) (:#)
        convertExpr (Null)              = (#:) "0" (:#)
        convertExpr (Value v)           = (#:) (convertVar v) (:#)
        convertExpr (Assign v e)        = (#:) (convertVar v) (:#) `ASSIGN` convertExpr e
        convertExpr (Neq x1 x2)         = (#:) (((#:) (fullQual x1) (:#)) `NOTEQUAL` ((#:) (fullQual x2) (:#))) (:#)
        convertExpr (SafeIndex a i b d) = (#:) (bounds_check_e `QUESTION` (Trio (fromList [array_index_e]) COLON ((#:) (show d) (:#)))) (:#)
            where bounds_check_e = (#:) (((#:) (convertVar i) (:#)) `LESS_THAN` ((#:) (convertVar b) (:#))) (:#)
                  array_index_e = (#:) (EPostfixExpression (convertVar a) LEFTSQUARE (fromList [(#:) (convertVar i) (:#)]) RIGHTSQUARE) (:#)

        convertVar :: Var (FullyQualAndEvent Identifier) -> PostfixExpression
        convertVar (Var x)     = (#:) (fullQual x) (:#)
        convertVar (SumVar x)  = (#:) (fullQual x) (:#) `DOT` id_field
        convertVar (Field (SumVar x) f) = (#:) (fullQual x) (:#) `DOT` event_field `DOT` evtQual f
        convertVar (Field v f) = convertVar v `ARROW` fullQual f

        convertTName :: Tagged Identifier -> Identifier
        convertTName (TagEvent x) = rename aliases $ x +-+ "t"
        convertTName t = foldMap id t

        apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
        apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
        apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

passMangle :: Alias QualifiedName -> SmudgeIR QualifiedName -> Except Fault (Alias Identifier, SmudgeIR (FullyQualAndEvent Identifier))
passMangle aliases ir = do
    aliases' <- traverseAliases convertQName aliases
    ir' <- traverse (traverse convertFQAE) ir
    return (aliases', ir')
    where
        convertFQAE :: QualifiedName -> Except Fault (FullyQualAndEvent Identifier)
        convertFQAE x = FullyQualAndEvent <$> convertQName x <*> convertQName (extractWith seq (qualify . (,) (qualify "e")) x)
        convertQName :: Qualifiable q => q -> Except Fault Identifier
        convertQName q = check $ finalize $ extractWith joinIdentifier (rawtest isSimpleUnderscore &&& mangle mangleIdentifier) $ rename aliases $ qualify q
            where joinIdentifier (su, x) (True, y)  = (su, x +-+ "" +-+ y)
                  joinIdentifier (su, x) (False, y) = (su, x +-+ y)
                  finalize (su, x) = if any isDigit (take 1 x) then snd $ joinIdentifier (False, "") (su, x) else x
                  check x = if not $ isPrefixOf "_" x then return x
                            else throwE (RuntimeFault ERROR $ "Attempt to emit leading-underscore id which is a reserved id class in C89 §4.1.2: " ++ x)

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
                 "generate stub implementation."])
    generate os cfg gswust outputTarget = do
        let convertTU = fmap fromList . mapExceptT (return . runIdentity)
        hdr <- convertTU $ convertIR True False <$> (passMangle aliases $ lowerSymTab gs $ filterBind syms Exported)
        src <- convertTU $ concat <$> sequence [convertIR True True <$> (passMangle aliases $ lower cfg ([g], syms)) | g <- gs]
        ext <- convertTU $ convertIR True False <$> (passMangle aliases $ lowerSymTab [] $ filterBind syms External)
        exs <- convertTU $ convertIR False True <$> (passMangle aliases $ lowerSymTab [] $ filterBind syms External)
        liftIO $ sequence $ [writeTranslationUnit (renderHdr hdr []) headerName,
            writeTranslationUnit (renderSrc src [extHdrName, headerName]) outputName,
            writeTranslationUnit (renderHdr ext [headerName]) extHdrName] ++
            if stubs then [writeTranslationUnit (renderStubs exs [extHdrName, headerName]) extSrcName] else []
        where (gs, aliases, syms) = gswust
              writeTranslationUnit render fp = (render fp) >>= (writeFile fp) >> (return fp)
              renderLeaderTrailer leader trailer u includes fp = leader includes fp <++ (renderPretty u ++ trailer)
              renderHdr = renderLeaderTrailer hdrLeader hdrTrailer
              renderSrc = renderLeaderTrailer srcLeader srcTrailer
              renderStubs = renderLeaderTrailer stubLeader srcTrailer
              outputBaseName = dropExtension outputTarget
              outputExtName = outputBaseName ++ "_ext"
              renames = [o | o@(TargetPath _ _ _) <- os]
              headerName = normalise $ head $ [f | TargetPath Header IntFile f <- renames] ++ [(outputBaseName <.> "h")]
              outputName = normalise $ head $ [f | TargetPath Source IntFile f <- renames] ++ [(outputBaseName <.> "c")]
              extHdrName = normalise $ head $ [f | TargetPath Header ExtFile f <- renames] ++ [(outputExtName <.> "h")]
              extSrcName = normalise $ head $ [f | TargetPath Source ExtFile f <- renames] ++ [(outputExtName <.> "c")]

              ward = "/* This file is generated by Smudge. Do not edit it. */\n"
              genIncludes :: [FilePath] -> FilePath -> IO [Char]
              genIncludes includes includer = liftA concat $ sequence $ map (mkInclude includer) includes

              mkInclude includer include =
                do
                  relativeInclude <- relPath (takeDirectory includer) include
                  return $ concat ["#include \"", relativeInclude, "\"\n"]

              srcLeader includes fp =
                do
                  gennedIncludes <- genIncludes includes fp
                  return $ concat [ward, gennedIncludes]

              stubLeader includes fp =
                do
                  gennedIncludes <- genIncludes includes fp
                  return $ concat [gennedIncludes]

              srcTrailer = ""
              reinclusionName fp = concat ["__", map (\a -> (if a == '.' then '_' else a)) (takeFileName fp), "__"]
              hdrLeader includes fp =
                do
                  gennedIncludes <- genIncludes includes fp
                  return $ concat [ward, "#ifndef ", reinclusionName fp, "\n", "#define ", reinclusionName fp, "\n",
                                   gennedIncludes]
              hdrTrailer = "#endif\n"
              stubs = not $ null $ filter (==GenStubs) os
