{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..))
import Grammars.C89
import Model (EnterExitState(..), HappeningFlag(..), Happening(..), QualifiedName(..), mangleWith, SymbolType(..), Binding(..),SymbolTable, qName)
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

extendMangle :: Identifier -> String -> Identifier
extendMangle s s' = s +-+ mangleIdentifier s'

mangleQName :: QualifiedName -> Identifier
mangleQName q = mangleWith (+-+) mangleIdentifier q

mangleQEventName :: QualifiedName -> Identifier
mangleQEventName qn = if null $ mangleQName qn then "" else mangleQName qn +-+ "t"

transitionFunctionDeclaration :: StateMachine -> Event -> Declaration
transitionFunctionDeclaration (StateMachine smName) e =
    Declaration
    (fromList [A STATIC, B VOID])
    (Just $ fromList [InitDeclarator (Declarator Nothing (PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList (fromList $ [ParameterDeclaration (fromList [B VOID]) Nothing]) Nothing)
          RIGHTPAREN)) Nothing])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName
        tType = "exit"
        f_name = smMangledName +-+ "ANY_STATE" +-+ tType


transitionFunction :: StateMachine -> Event -> [State] -> FunctionDefinition
transitionFunction (StateMachine smName) EventExit ss =
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
        smMangledName = mangleIdentifier smName
        tType = "exit"
        f_name = smMangledName +-+ "ANY_STATE" +-+ tType
        state_var = (#:) (smMangledName +-+ "state") (:#)
        ssMangled = [smMangledName +-+ mangleIdentifier s | (State s) <- ss]
        case_stmt s = let state_case = (#:) s (:#)
                          state_evt_handler = (#:) (s +-+ tType) (:#)
                          call_state_evt_handler = (#:) (apply state_evt_handler []) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

initializeFunction :: StateMachine -> State -> FunctionDefinition
initializeFunction (StateMachine smName) (State s) =
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
        smMangledName = mangleIdentifier smName
        sMangled = smMangledName +-+ mangleIdentifier s
        state_var = smMangledName +-+ "state"
        f_name = smMangledName +-+ "initialize"
        init_var = "initialized"
        init_init = "INITIALIZED"
        init_uninit = "UNINITIALIZED"
        assign_state = ((#:) state_var (:#)) `ASSIGN` ((#:) sMangled (:#))
        enter_f = (#:) (sMangled +-+ "enter") (:#)
        call_enter = (#:) (apply enter_f []) (:#)
        init_check = (#:) ((#:) init_init (:#) `NOTEQUAL` (#:) init_var (:#)) (:#)
        init_set = (#:) init_var (:#) `ASSIGN` (#:) init_init (:#)
        side_effects = [assign_state, call_enter, init_set]

handleStateEventFunction :: StateMachine -> State -> Happening -> State -> SymbolTable -> FunctionDefinition
handleStateEventFunction sm@(StateMachine smName) st h st' syms =
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
        smMangledName = mangleIdentifier smName
        sMangledName = case st of
                          State s -> mangleIdentifier s
                          StateAny -> "ANY_STATE"
        destStateMangledName = case st' of
                                  State s' -> mangleIdentifier s'
                                  StateAny -> "ANY_STATE"
        evMangledName = case event h of
                            (Event evName) -> mangleIdentifier evName
                            (EventEnter) -> "enter"
                            (EventExit) -> "exit"
                            (EventAny) -> "any"
        hasPs = case event h of
                        (Event _) -> True
                        otherwise -> False
        f_name = smMangledName +-+ sMangledName +-+ evMangledName
        event_type = smMangledName +-+ evMangledName +-+ "t"
        event_var = "e"
        event_ex = (#:) event_var (:#)
        dest_state = (#:) (smMangledName +-+ destStateMangledName) (:#)
        state_var = (#:) (smMangledName +-+ "state") (:#)
        assign_state = (state_var `ASSIGN` dest_state)
        exit_f = (#:) (smMangledName +-+ sMangledName +-+ "exit") (:#)
        enter_f = (#:) (smMangledName +-+ destStateMangledName +-+ "enter") (:#)
        call_exit = (#:) (apply exit_f []) (:#)
        call_enter = (#:) (apply enter_f []) (:#)

        psOf (_, FunctionSym p _) | mangleQEventName p == event_type = [event_ex]
        psOf (_, FunctionSym (QualifiedName (_:_)) _)                = [(#:) "0" (:#)]
        psOf _                                                       = []
        apply_se (FuncEvent se _) = undefined -- See ticket #15, harder than it seems at first.
        apply_se se = (#:) (apply ((#:) (mangleQName (qName sm se)) (:#)) (psOf (syms ! (qName sm se)))) (:#)
        side_effects = case h of
                          (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ [call_exit, assign_state, call_enter]
                          (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

handleEventFunction :: Bool -> StateMachine -> Event -> [State] -> [State] -> [State] -> FunctionDefinition
handleEventFunction debug (StateMachine smName) (Event evName) ss anys unss =
    makeFunction (fromList [B VOID]) [] f_name [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                            (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_initialize]) SEMICOLON,
                          SStatement $ SWITCH LEFTPAREN (fromList [state_var]) RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
                                       Nothing
                                       (Just $ fromList $ concat ([[LStatement $ case_stmt s evName [event_ex], JStatement $ BREAK SEMICOLON] | s <- ssMangled]
                                                                 ++ [[LStatement $ case_stmt s "any" [], JStatement $ BREAK SEMICOLON] | s <- anysMangled]
                                                                 ++ [[LStatement $ unhd_stmt s, JStatement $ BREAK SEMICOLON] | s <- unssMangled])
                                                                 ++ [LStatement $ DEFAULT COLON $
                                                                     EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON,
                                                                     JStatement $ BREAK SEMICOLON])
                                       RIGHTCURLY])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        ssMangled = [smMangledName +-+ mangleIdentifier s | (State s) <- ss]
        anysMangled = [extendMangle smMangledName s | (State s) <- anys]
        unssMangled = [smMangledName +-+ mangleIdentifier s | (State s) <- unss]
        evMangledName = mangleIdentifier evName
        f_name = smMangledName +-+ evMangledName
        event_type = f_name +-+ "t"
        event_var = "e"
        event_ex = (#:) event_var (:#)
        state_var = (#:) (smMangledName +-+ "state") (:#)
        unhandled = (#:) (smMangledName +-+ "UNHANDLED_EVENT" +-+ evMangledName) (:#)
        initialize = (#:) (smMangledName +-+ "initialize") (:#)
        call_unhandled = (#:) (apply unhandled [event_ex]) (:#)
        call_initialize = (#:) (apply initialize []) (:#)
        unhd_stmt s = let state_case = (#:) s (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON
        case_stmt s en es = let state_case = (#:) s (:#)
                                state_evt_handler = (#:) (extendMangle s en) (:#)
                                call_state_evt_handler = (#:) (apply state_evt_handler es) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

unhandledEventFunction :: Bool -> Bool -> StateMachine -> Event -> FunctionDefinition
unhandledEventFunction debug any_handles (StateMachine smName) (Event evName) =
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
        smMangledName = mangleIdentifier smName
        evMangledName = mangleIdentifier evName
        evname_e = (#:) (show evName) (:#)
        f_name = smMangledName +-+ "UNHANDLED_EVENT" +-+ evMangledName
        event_type = smMangledName +-+ evMangledName +-+ "t"
        event_var = "e"
        assert_f = (#:) (if debug then "printf_assert" else "assert") (:#)
        assert_s = (#:) (show (smName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (smMangledName +-+ "State_name") (:#)
        state_var = (#:) (smMangledName +-+ "state") (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_assert_f = (#:) (apply assert_f (if debug then [assert_s, call_sname_f, name_ex] else [])) (:#)
        any_f = (#:) (smMangledName +-+ "ANY_STATE" +-+ evMangledName) (:#)
        call_any_f = (#:) (apply any_f [(#:) event_var (:#)]) (:#)

stateNameFunction :: StateMachine -> [State] -> FunctionDefinition
stateNameFunction (StateMachine smName) ss =
    makeFunction (fromList [A STATIC, C CONST, B CHAR]) [POINTER Nothing] f_name
                                           [ParameterDeclaration (fromList [B $ TypeSpecifier smMangledName])
                                            (Just $ Left $ Declarator Nothing $ IDirectDeclarator state_var)] 
    (CompoundStatement
    LEFTCURLY
        (Just $ fromList [Declaration (fromList [A STATIC, C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER Nothing]) $
                                                                        CDirectDeclarator (IDirectDeclarator names_var) LEFTSQUARE Nothing RIGHTSQUARE)
                                                        (Just $ Pair EQUAL (LInitializer LEFTCURLY
                                                                                         (fromList [AInitializer ((#:) (show s) (:#)) | (State s) <- ss])
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
        smMangledName = mangleIdentifier smName +-+ "State"
        count_var = "state_count"
        state_var = "s"
        names_var = "state_name"
        f_name = smMangledName +-+ "name"
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

handleStateEventDeclaration :: StateMachine -> State -> Event -> Declaration
handleStateEventDeclaration (StateMachine smName) st e =
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
        smMangledName = mangleIdentifier smName
        sMangledName = case st of
                          State s -> mangleIdentifier s
                          StateAny -> "ANY_STATE"
        evMangledName = case e of
                            (Event evName) -> mangleIdentifier evName
                            (EventEnter) -> "enter"
                            (EventExit) -> "exit"
                            (EventAny) -> "any"
        hasPs = case e of
                        (Event _) -> True
                        otherwise -> False
        f_name = smMangledName +-+ sMangledName +-+ evMangledName
        event_type = smMangledName +-+ evMangledName +-+ "t"

handleEventDeclaration :: StateMachine -> Event -> Declaration
handleEventDeclaration (StateMachine smName) (Event evName) =
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
        smMangledName = mangleIdentifier smName
        evMangledName = mangleIdentifier evName
        f_name = smMangledName +-+ evMangledName
        event_type = smMangledName +-+ evMangledName +-+ "t"

stateVarDeclaration :: StateMachine -> State -> Declaration
stateVarDeclaration (StateMachine smName) (State s) =
    Declaration
    (fromList [A STATIC, B $ TypeSpecifier smEnum])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator state_var)) 
                                     (Just $ Pair EQUAL $ AInitializer ((#:) sMangled (:#)))])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName
        smEnum = smMangledName +-+ "State"
        sMangled = smMangledName +-+ mangleIdentifier s
        state_var = smMangledName +-+ "state"

stateEnum :: StateMachine -> [State] -> Declaration
stateEnum (StateMachine smName) ss =
    Declaration
    (fromList [A TYPEDEF,
               B (makeEnum smMangledName ssMangled)])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator smMangledName)) Nothing])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName +-+ "State"
        ssMangled = [mangleIdentifier smName +-+ mangleIdentifier s | (State s) <- ss]

makeEnum :: Identifier -> [Identifier] -> TypeSpecifier
makeEnum smName [] = ENUM (Left $ smName)
makeEnum smName ss = 
    ENUM (Right (Quad (if null smName then Nothing else Just $ smName)
    LEFTCURLY
    (fromList [Enumerator s Nothing | s <- ss])
    RIGHTCURLY))

eventStruct :: StateMachine -> Event -> Declaration
eventStruct (StateMachine smName) (Event evName) =
    Declaration
    (fromList [A TYPEDEF,
               B (makeStruct event_type [])])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator event_type)) Nothing])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName
        evMangledName = mangleIdentifier evName
        event_type = smMangledName +-+ evMangledName +-+ "t"

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

makeFunctionDeclaration :: QualifiedName -> SymbolType -> Declaration
makeFunctionDeclaration q (FunctionSym p r) =
    Declaration
    (fromList [A EXTERN, B $ result ])
    (Just $ fromList [InitDeclarator (makeFunctionDeclarator ps f_name params) Nothing])
    SEMICOLON
    where
        r_name = mangleQEventName r
        p_name = mangleQEventName p
        result = case r_name of "" -> VOID; t -> TypeSpecifier t
        ps = case r_name of "" -> []; _ -> [POINTER Nothing]
        f_name = mangleQName q
        params = case p_name of
                    "" -> [ParameterDeclaration (fromList [B VOID]) Nothing]
                    t -> [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier t])
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
              tue = [ExternalDeclaration $ Right $ makeFunctionDeclaration name ftype | (name, (External, ftype)) <- toList syms]
              tus = [[ExternalDeclaration $ Right $ stateEnum sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration sm $ initial g]
                     ++ [ExternalDeclaration $ Right $ transitionFunctionDeclaration sm EventExit | (_, EnterExitState {st = StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration sm s e
                         | (n, EnterExitState {st = s}) <- labNodes g, e <- (map (event . edgeLabel) $ out g n), case s of State _ -> True; StateAny -> True; _ -> False]
                     ++ (if debug then [ExternalDeclaration $ Left $ stateNameFunction sm $ states g] else [])
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventExit
                         $ [st | (_, EnterExitState {st, ex = (_:_)}) <- labNodes g] | (_, EnterExitState {st = StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Left $ unhandledEventFunction debug (not $ null [st | st@StateAny <- states_handling e g]) sm e | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ initializeFunction sm $ initial g]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction debug sm e ss (anys g \\ ss) ((states g \\ ss) \\ (anys g))
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
              gs = fst gswust
              syms = snd gswust
              initial g = head [st ese | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n, (Just ese) <- [lab g n']]
              states g = [st ees | (_, ees) <- labNodes g]
              states_handling e g = [st ees | (n, ees) <- labNodes g, (_, _, Happening {event}) <- out g n, event == e]
              anys g = states_handling EventAny g
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
