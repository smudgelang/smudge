{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (
  Annotated(..),
  StateMachineDeclarator(..),
  State(..),
  Event(..),
  Function(..),
  SideEffect(..),
  )
import Grammars.C89
import Model (
  EnterExitState(..),
  HappeningFlag(..),
  Happening(..),
  QualifiedName(..),
  Tag(TagEvent),
  TaggedName,
  untag,
  mangleWith,
  disqualifyTag,
  nestCookedInScope,
  qName,
  )
import Semantics.Solver (Ty(..), Binding(..), SymbolTable, insertExternalSymbol)
import Trashcan.FilePath (relPath)
import Unparsers.C89 (renderPretty)

import Data.Graph.Inductive.Graph (labNodes, lab, out, suc, insEdges, nodes, delNodes)
import Data.List (intercalate, (\\))
import Data.Map (insertWith, empty, toList, (!))
import qualified Data.Map (null)
import Data.Text (replace)
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension, takeDirectory, takeFileName, (<.>))

data CStaticOption = OutFile FilePath | Header FilePath | NoDebug
    deriving (Show, Eq)

apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

(+-+) :: Identifier -> Identifier -> Identifier
a +-+ b = a ++ "_" ++ b

mangleQName :: QualifiedName -> Identifier
mangleQName q = mangleWith (+-+) mangleIdentifier q

mangleTName :: TaggedName -> Identifier
mangleTName (TagEvent, q) = mangleQName q +-+ "t"
mangleTName t = mangleQName $ untag t

mangleEvWith :: (TaggedName -> Identifier) -> Event TaggedName -> Identifier
mangleEvWith f (Event evName) = f evName
mangleEvWith _ EventEnter = "enter"
mangleEvWith _ EventExit = "exit"
mangleEvWith _ EventAny = "any"

transitionFunctionDeclaration :: StateMachineDeclarator TaggedName -> Event TaggedName -> Declaration
transitionFunctionDeclaration (StateMachineDeclarator smName) e =
    Declaration
    (fromList [A STATIC, B VOID])
    (Just $ fromList [InitDeclarator (Declarator Nothing (PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList (fromList $ [ParameterDeclaration (fromList [B VOID]) Nothing]) Nothing)
          RIGHTPAREN)) Nothing])
    SEMICOLON
    where
        tType = "exit"
        f_name = mangleQName $ nestCookedInScope (nestCookedInScope (untag smName) "ANY_STATE") tType


transitionFunction :: StateMachineDeclarator TaggedName -> Event TaggedName -> [State TaggedName] -> FunctionDefinition
transitionFunction (StateMachineDeclarator smName) EventExit ss =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name [ParameterDeclaration (fromList [B VOID]) Nothing]
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [SStatement $ SWITCH LEFTPAREN (fromList [state_var]) RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
                                       Nothing
                                       (Just $ fromList $ concat ([[LStatement $ case_stmt s, JStatement $ BREAK SEMICOLON] | s <- ssMangled])
                                                                 ++ [LStatement $ DEFAULT COLON $
                                                                     JStatement $ BREAK SEMICOLON])
                                       RIGHTCURLY])
    RIGHTCURLY)
    where
        tType = "exit"
        f_name = mangleQName $ nestCookedInScope (nestCookedInScope (untag smName) "ANY_STATE") tType
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        ssMangled = [mangleTName s | (State s) <- ss]
        case_stmt s = let state_case = (#:) s (:#)
                          state_evt_handler = (#:) (s +-+ tType) (:#)
                          call_state_evt_handler = (#:) (apply state_evt_handler []) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

initializeFunction :: StateMachineDeclarator TaggedName -> State TaggedName -> FunctionDefinition
initializeFunction (StateMachineDeclarator smName) (State s) =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name [ParameterDeclaration (fromList [B VOID]) Nothing]
    (CompoundStatement
    LEFTCURLY
        (Just $ fromList [Declaration
                          (fromList [A STATIC, B (makeEnum "" [init_uninit, init_init])])
                          (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator init_var)) (Just $ Pair EQUAL (AInitializer ((#:) init_uninit (:#))))])
                           SEMICOLON])
        (Just $ fromList [SStatement $ IF LEFTPAREN (fromList [init_check]) RIGHTPAREN (CStatement $ CompoundStatement
                                       LEFTCURLY
                                            Nothing
                                            (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [se]) SEMICOLON | se <- side_effects])
                                       RIGHTCURLY)
                                       Nothing])
    RIGHTCURLY)
    where
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        f_name = mangleQName $ nestCookedInScope (untag smName) "initialize"
        init_var = "initialized"
        init_init = "INITIALIZED"
        init_uninit = "UNINITIALIZED"
        assign_state = state_var `ASSIGN` ((#:) (mangleTName s) (:#))
        enter_f = (#:) (mangleQName $ nestCookedInScope (untag s) "enter") (:#)
        call_enter = (#:) (apply enter_f []) (:#)
        init_check = (#:) ((#:) init_init (:#) `NOTEQUAL` (#:) init_var (:#)) (:#)
        init_set = (#:) init_var (:#) `ASSIGN` (#:) init_init (:#)
        side_effects = [assign_state, call_enter, init_set]

sName :: TaggedName -> State TaggedName -> QualifiedName
sName _ (State s) = untag s
sName smName StateAny  = nestCookedInScope (untag smName) "ANY_STATE"

handleStateEventFunction :: StateMachineDeclarator TaggedName -> State TaggedName -> Happening -> State TaggedName -> SymbolTable -> FunctionDefinition
handleStateEventFunction sm@(StateMachineDeclarator smName) st h st' syms =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name
                                   (if not hasPs then [ParameterDeclaration (fromList [B VOID]) Nothing]
                                    else [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                          (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)])
    (CompoundStatement
    LEFTCURLY
        Nothing
        (if null side_effects then Nothing else Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [se]) SEMICOLON | se <- side_effects])
    RIGHTCURLY)
    where
        sScope = nestCookedInScope $ sName smName st
        destStateMangledName = mangleQName $ sName smName st'
        hasPs = case event h of
                        (Event _) -> True
                        otherwise -> False
        f_name = mangleQName $ sScope $ mangleEvWith (mangleIdentifier . disqualifyTag) $ event h
        event_type = mangleEvWith mangleTName $ event h
        event_var = "e"
        event_ex = (#:) event_var (:#)
        dest_state = (#:) destStateMangledName (:#)
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        assign_state = (state_var `ASSIGN` dest_state)
        exit_f = (#:) (mangleQName $ sScope "exit") (:#)
        enter_f = (#:) (mangleQName $ nestCookedInScope (sName smName st') "enter") (:#)
        call_exit = (#:) (apply exit_f []) (:#)
        call_enter = (#:) (apply enter_f []) (:#)

        isEventTy :: Ty -> Event TaggedName -> Bool
        isEventTy (Ty _ a) (Event e) = a == e
        isEventTy _ _ = False

        psOf (Unary _ Void _) = []
        psOf (Unary _ p    _) = [if isEventTy p (event h) then event_ex else (#:) "0" (:#)]
        apply_se (f, FuncTyped _) = undefined -- See ticket #15, harder than it seems at first.
        apply_se (f, _) = (#:) (apply ((#:) (mangleTName f) (:#)) (psOf (syms ! f))) (:#)
        side_effects = case h of
                          (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ [call_exit, assign_state, call_enter]
                          (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

handleEventFunction :: Bool -> StateMachineDeclarator TaggedName -> Event TaggedName -> [State TaggedName] -> [State TaggedName] -> [State TaggedName] -> FunctionDefinition
handleEventFunction debug (StateMachineDeclarator smName) (Event evName) ss anys unss =
    makeFunction (fromList [B VOID]) [] f_name [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                            (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_initialize]) SEMICOLON,
                          SStatement $ SWITCH LEFTPAREN (fromList [state_var]) RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
                                       Nothing
                                       (Just $ fromList $ concat ([[LStatement $ case_stmt s evAlone [event_ex], JStatement $ BREAK SEMICOLON] | (State s) <- ss]
                                                                 ++ [[LStatement $ case_stmt s "any" [], JStatement $ BREAK SEMICOLON] | (State s) <- anys]
                                                                 ++ [[LStatement $ unhd_stmt s, JStatement $ BREAK SEMICOLON] | (State s) <- unss])
                                                                 ++ [LStatement $ DEFAULT COLON $
                                                                     EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON,
                                                                     JStatement $ BREAK SEMICOLON])
                                       RIGHTCURLY])
    RIGHTCURLY)
    where
        evAlone = mangleIdentifier $ disqualifyTag evName
        f_name = mangleQName $ untag evName
        event_type = mangleTName evName
        event_var = "e"
        event_ex = (#:) event_var (:#)
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        unhandled = (#:) (mangleQName $ nestCookedInScope (nestCookedInScope (untag smName) "UNHANDLED_EVENT") evAlone) (:#)
        initialize = (#:) (mangleQName $ nestCookedInScope (untag smName) "initialize") (:#)
        call_unhandled = (#:) (apply unhandled [event_ex]) (:#)
        call_initialize = (#:) (apply initialize []) (:#)
        unhd_stmt s = let state_case = (#:) (mangleTName s) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON
        case_stmt s en es = let state_case = (#:) (mangleTName s) (:#)
                                state_evt_handler = (#:) (mangleQName $ nestCookedInScope (untag s) en) (:#)
                                call_state_evt_handler = (#:) (apply state_evt_handler es) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

unhandledEventFunction :: Bool -> Bool -> StateMachineDeclarator TaggedName -> Event TaggedName -> FunctionDefinition
unhandledEventFunction debug any_handles (StateMachineDeclarator smName) (Event evName) =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                                          (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
    (CompoundStatement
    LEFTCURLY
        (if any_handles || not debug then Nothing else
          (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                        (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator name_var)
                                                          (Just $ Pair EQUAL $ AInitializer evname_e)])
                                        SEMICOLON]))
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [if any_handles then call_any_f else call_assert_f]) SEMICOLON])
    RIGHTCURLY)
    where
        name_var = "event_name"
        name_ex = (#:) name_var (:#)
        evAlone = mangleIdentifier $ disqualifyTag evName
        evname_e = (#:) (show $ disqualifyTag evName) (:#)
        f_name = mangleQName $ nestCookedInScope (nestCookedInScope (untag smName) "UNHANDLED_EVENT") evAlone
        event_type = mangleTName evName
        event_var = "e"
        assert_f = (#:) (if debug then "printf_assert" else "assert") (:#)
        assert_s = (#:) (show (disqualifyTag smName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (mangleQName $ nestCookedInScope (untag smName) "State_name") (:#)
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_assert_f = (#:) (apply assert_f (if debug then [assert_s, call_sname_f, name_ex] else [])) (:#)
        any_f = (#:) (mangleQName $ nestCookedInScope (nestCookedInScope (untag smName) "ANY_STATE") (mangleIdentifier $ disqualifyTag evName)) (:#)
        call_any_f = (#:) (apply any_f [(#:) event_var (:#)]) (:#)

stateNameFunction :: StateMachineDeclarator TaggedName -> [State TaggedName] -> FunctionDefinition
stateNameFunction (StateMachineDeclarator smName) ss =
    makeFunction (fromList [A STATIC, C CONST, B CHAR]) [POINTER Nothing] f_name
                                           [ParameterDeclaration (fromList [B $ TypeSpecifier smEnum])
                                            (Just $ Left $ Declarator Nothing $ IDirectDeclarator state_var)] 
    (CompoundStatement
    LEFTCURLY
        (Just $ fromList [Declaration (fromList [A STATIC, C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER $ Just $ fromList [CONST]]) $
                                                                        CDirectDeclarator (IDirectDeclarator names_var) LEFTSQUARE Nothing RIGHTSQUARE)
                                                        (Just $ Pair EQUAL (LInitializer LEFTCURLY
                                                                                         (fromList [AInitializer ((#:) (show $ disqualifyTag s) (:#)) | (State s) <- ss])
                                                                                         Nothing
                                                                                         RIGHTCURLY))])
                                      SEMICOLON,
                          Declaration (fromList [A STATIC, C CONST, B UNSIGNED, B INT])
                                      (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator count_var))
                                                        (Just $ Pair EQUAL (AInitializer names_count_e ))])
                                      SEMICOLON])
        (Just $ fromList [JStatement $ RETURN (Just $ fromList [safe_array_index_e]) SEMICOLON])
    RIGHTCURLY)
    where
        smEnum = mangleQName $ nestCookedInScope (untag smName) "State"
        count_var = "state_count"
        state_var = "s"
        names_var = "state_name"
        f_name = mangleQName $ nestCookedInScope (untag smName) "State_name"
        names_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier names_var]) Nothing) RIGHTPAREN) (:#)
        ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                     (Just $ AbstractDeclarator $ This $ fromList [POINTER Nothing])) RIGHTPAREN) (:#)
        names_count_e = (#:) (names_size_e `DIV` ptr_size_e) (:#)
        count_var_e = (#:) count_var (:#)
        state_var_e = (#:) state_var (:#)
        names_var_e = (#:) names_var (:#)
        default_state = (#:) (show "INVALID_STATE") (:#)
        bounds_check_e = (#:) (state_var_e `LESS_THAN` count_var_e) (:#)
        array_index_e = (#:) (EPostfixExpression names_var_e LEFTSQUARE (fromList [(#:) state_var_e (:#)]) RIGHTSQUARE) (:#)
        safe_array_index_e = (#:) (bounds_check_e `QUESTION` (Trio (fromList [array_index_e]) COLON default_state)) (:#)

currentStateNameFunction :: Bool -> StateMachineDeclarator TaggedName -> FunctionDefinition
currentStateNameFunction debug (StateMachineDeclarator smName) = 
    makeFunction (fromList [C CONST, B CHAR]) [POINTER Nothing] f_name [ParameterDeclaration (fromList [B VOID]) Nothing]
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [JStatement $ RETURN (Just $ fromList [if debug then call_sname_f else ((#:) (show "") (:#))]) SEMICOLON])
    RIGHTCURLY)
    where
        f_name = mangleQName $ nestCookedInScope (untag smName) "Current_state_name"
        sname_f  = (#:) (mangleQName $ nestCookedInScope (untag smName) "State_name") (:#)
        state_var = (#:) (mangleQName $ nestCookedInScope (untag smName) "state") (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)

handleStateEventDeclaration :: StateMachineDeclarator TaggedName -> State TaggedName -> Event TaggedName -> Declaration
handleStateEventDeclaration (StateMachineDeclarator smName) st e =
    Declaration
    (fromList [A STATIC, B VOID])
    (Just $ fromList [InitDeclarator (Declarator Nothing (PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList
                         (fromList (if not hasPs then [ParameterDeclaration (fromList [B VOID]) Nothing]
                                    else [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                          (Just $ Right $ AbstractDeclarator $ This $ fromList [POINTER Nothing])]))
                         Nothing)
          RIGHTPAREN)) Nothing])
    SEMICOLON
    where
        sScope = nestCookedInScope $ sName smName st
        hasPs = case e of
                        (Event _) -> True
                        otherwise -> False
        f_name = mangleQName $ sScope $ mangleEvWith (mangleIdentifier . disqualifyTag) e
        event_type = mangleEvWith mangleTName e

handleEventDeclaration :: StateMachineDeclarator TaggedName -> Event TaggedName -> Declaration
handleEventDeclaration (StateMachineDeclarator smName) (Event evName) =
    Declaration
    (fromList [B VOID])
    (Just $ fromList [InitDeclarator (Declarator Nothing (PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList
                         (fromList [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                    (Just $ Right $ AbstractDeclarator $ This $ fromList [POINTER Nothing])])
                         Nothing)
          RIGHTPAREN)) Nothing])
    SEMICOLON
    where
        f_name = mangleQName $ untag evName
        event_type = mangleTName evName

stateVarDeclaration :: StateMachineDeclarator TaggedName -> State TaggedName -> Declaration
stateVarDeclaration (StateMachineDeclarator smName) (State s) =
    Declaration
    (fromList [A STATIC, B $ TypeSpecifier smEnum])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator state_var)) 
                                     (Just $ Pair EQUAL $ AInitializer ((#:) sMangled (:#)))])
    SEMICOLON
    where
        smEnum = mangleQName $ nestCookedInScope (untag smName) "State"
        sMangled = mangleTName s
        state_var = mangleQName $ nestCookedInScope (untag smName) "state"

stateEnum :: StateMachineDeclarator TaggedName -> [State TaggedName] -> Declaration
stateEnum (StateMachineDeclarator smName) ss =
    Declaration
    (fromList [A TYPEDEF,
               B (makeEnum smEnum ssMangled)])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator smEnum)) Nothing])
    SEMICOLON
    where
        smEnum = mangleQName $ nestCookedInScope (untag smName) "State"
        ssMangled = [mangleTName s | (State s) <- ss]

makeEnum :: Identifier -> [Identifier] -> TypeSpecifier
makeEnum smName [] = ENUM (Left $ smName)
makeEnum smName ss = 
    ENUM (Right (Quad (if null smName then Nothing else Just $ smName)
    LEFTCURLY
    (fromList [Enumerator s Nothing | s <- ss])
    RIGHTCURLY))

eventStruct :: StateMachineDeclarator TaggedName -> Event TaggedName -> Declaration
eventStruct (StateMachineDeclarator smName) (Event evName) =
    Declaration
    (fromList [A TYPEDEF,
               B (makeStruct event_type [])])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator event_type)) Nothing])
    SEMICOLON
    where
        event_type = mangleTName evName

makeStruct :: Identifier -> [(SpecifierQualifierList, Identifier)] -> TypeSpecifier
makeStruct smName [] = STRUCT (Left $ smName)
makeStruct smName ss = 
    STRUCT (Right (Quad (Just $ smName)
    LEFTCURLY
    (fromList [StructDeclaration sqs (fromList [StructDeclarator $ This $ Declarator Nothing $ IDirectDeclarator id]) SEMICOLON | (sqs, id) <- ss])
    RIGHTCURLY))

makeFunctionDeclarator :: [Pointer] -> Identifier -> [ParameterDeclaration] -> Declarator
makeFunctionDeclarator ps f_name params =
    Declarator (if null ps then Nothing else Just $ fromList ps)
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList (fromList params) Nothing)
          RIGHTPAREN

makeFunctionDeclaration :: TaggedName -> Ty -> Declaration
makeFunctionDeclaration n (Unary b p r) =
    Declaration
    (fromList $ binding ++ [B result])
    (Just $ fromList [InitDeclarator (makeFunctionDeclarator ps f_name params) Nothing])
    SEMICOLON
    where
        binding = case b of External -> [A EXTERN]; _ -> []
        result = case r of Void -> VOID; Ty _ t -> TypeSpecifier $ mangleTName t
        ps = case r of Void -> []; _ -> [POINTER Nothing]
        f_name = mangleTName n
        params = case p of
                    Void -> [ParameterDeclaration (fromList [B VOID]) Nothing]
                    Ty _ t -> [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier $ mangleTName t])
                               (Just $ Right $ AbstractDeclarator (This $ fromList [POINTER Nothing]))]

makeFunction :: DeclarationSpecifiers -> [Pointer] -> Identifier -> [ParameterDeclaration] -> CompoundStatement -> FunctionDefinition
makeFunction dss ps f_name params body =
    Function
    (Just dss)
    (makeFunctionDeclarator ps f_name params)
    Nothing
    body

instance Backend CStaticOption where
    options = ("c",
               [Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["h"] (ReqArg Header "FILE")
                 "The name of the target header file if not derived from source file.",
                Option [] ["no-debug"] (NoArg NoDebug)
                 "Don't generate debugging information"])
    generate os gswust inputName = sequence $ [writeTranslationUnit (renderHdr hdr []) (headerName os),
                                         writeTranslationUnit (renderSrc src [extHdrName os, headerName os]) (outputName os)]
                                         ++ [writeTranslationUnit (renderHdr ext [headerName os]) (extHdrName os) | not $ null tue]
        where src = fromList $ concat tus
              ext = fromList tue
              hdr = fromList $ concat tuh
              tuh = [[ExternalDeclaration $ Right $ eventStruct sm e
                         | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Right $ handleEventDeclaration sm e
                         | (e, _) <- toList $ events g]
                     | (sm, g) <- gs'']
                    ++ [[ExternalDeclaration $ Right $ makeFunctionDeclaration name ftype | (name, ftype) <- toList externs]]
              tue = [ExternalDeclaration $ Right $ makeFunctionDeclaration name ftype | (name, ftype@(Unary External _ _)) <- toList syms]
              tus = [[ExternalDeclaration $ Right $ stateEnum sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration sm $ initial g]
                     ++ [ExternalDeclaration $ Right $ transitionFunctionDeclaration sm EventExit | (_, EnterExitState {st = StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration sm s e
                         | (n, EnterExitState {st = s}) <- labNodes g, e <- (map (event . edgeLabel) $ out g n), case s of State _ -> True; StateAny -> True; _ -> False]
                     ++ (if debug then [ExternalDeclaration $ Left $ stateNameFunction sm $ states g] else [])
                     ++ [ExternalDeclaration $ Left $ currentStateNameFunction debug sm]
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventExit
                         $ [st | (_, EnterExitState {st, ex = (_:_)}) <- labNodes g] | (_, EnterExitState {st = StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Left $ unhandledEventFunction debug (any_handles e g) sm e | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ initializeFunction sm $ initial g]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction debug sm e ss (anys e g \\ ss) ((states g \\ ss) \\ (anys e g))
                         | (e, ss) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s' syms
                         | (n, EnterExitState {st = s}) <- labNodes g, (_, n', h) <- out g n, Just EnterExitState {st = s'} <- [lab g n'], case s of State _ -> True; StateAny -> True; _ -> False, case s' of State _ -> True; StateAny -> True; _ -> False]
                     | (sm, g) <- gs'']
              gs'' = [(sm, insEdges [(n, n, Happening EventEnter en [NoTransition])
                                     | (n, EnterExitState {en, st = State _}) <- labNodes $ delNodes [n | n <- nodes g, (_, _, Happening EventEnter _ _) <- out g n] g] g)
                      | (sm, g) <- gs']
              gs'  = [(sm, insEdges [(n, n, Happening EventExit ex [NoTransition])
                                     | (n, EnterExitState {st = State _, ex}) <- labNodes $ delNodes [n | n <- nodes g, (_, _, Happening EventExit _ _) <- out g n] g] g)
                      | (sm, g) <- gs]
              gs = [(smd, g) | (Annotated _ smd, g) <- fst gswust]
              syms :: SymbolTable
              syms = insertExternalSymbol (snd gswust) "assert" [] ""
              externs :: SymbolTable  -- Sorry
              externs = foldl (\syms sym -> insertExternalSymbol syms sym [] "const char") empty [mangleQName $ nestCookedInScope (untag smName) "Current_state_name" | ((StateMachineDeclarator smName), _) <- gs'']
              initial g = head [st ese | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n, (Just ese) <- [lab g n']]
              states g = [st ees | (_, ees) <- labNodes g]
              anys e g = if any_handles e g then [] else states_handling EventAny g
              any_handles e g = (not $ null [st | st@StateAny <- states_handling e g])
              states_handling e g = [st ees | (n, ees) <- labNodes g, (_, _, Happening {event}) <- out g n, event == e]
              events g = foldl insert_event empty [(h, st ees) | (n, ees) <- labNodes g, (_, _, h) <- out g n]
              insert_event m ((Happening e@(Event _) _ _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m ((Happening e@(Event _) _ _), s@StateAny)  = insertWith (flip (++)) e [s] m
              insert_event m                                          _ = m
              edgeLabel (_, _, l) = l
              writeTranslationUnit render fp = (writeFile fp (render fp)) >> (return fp)
              renderHdr u includes fp = hdrLeader includes fp ++ renderPretty u ++ hdrTrailer
              renderSrc u includes _ = srcLeader includes ++ renderPretty u ++ srcTrailer
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName ((dropExtension inputName) <.> "c") xs
              headerName ((Header f):_) = f
              headerName xs = getFirstOrDefault headerName ((dropExtension inputName) <.> "h") xs
              extHdrName xs = getFirstOrDefault extHdrName (((dropExtension inputName) ++ "_ext") <.> "h") xs
              headerIncludePath = relPath (takeDirectory $ outputName os) (headerName os)
              doDebug ((NoDebug):_) = False
              doDebug xs = getFirstOrDefault doDebug True xs
              mkInclude f = concat ["#include \"", f, "\"\n"]
              genIncludes includes = concat $ map mkInclude includes
              srcLeader = genIncludes
              srcTrailer = ""
              reinclusionName fp = concat ["__", map (\a -> (if a == '.' then '_' else a)) (takeFileName fp), "__"]
              hdrLeader includes fp = concat ["#ifndef ", reinclusionName fp, "\n", "#define ", reinclusionName fp, "\n", genIncludes includes]
              hdrTrailer = "#endif\n"
              debug = doDebug os
