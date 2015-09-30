{-# LANGUAGE NamedFieldPuns #-}

module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..))
import Grammars.C89
import Model (EnterExitState(..), HappeningFlag(..), Happening(..))
import Trashcan.FilePath (relPath)
import Unparsers.C89 (renderPretty)

import Data.Graph.Inductive.Graph (labNodes, lab, out, suc)
import Data.List (intercalate, (\\))
import Data.Map (insertWith, empty, toList)
import Data.Text (replace)
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension, takeDirectory, takeFileName, (<.>))

data CStaticOption = OutFile FilePath | Header FilePath | NoDebug
    deriving (Show, Eq)

apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

extendMangledIdentifier :: Identifier -> [String] -> Identifier
extendMangledIdentifier s ss = intercalate "_" $ s : map mangleIdentifier ss

transitionFunction :: StateMachine -> Event -> [State] -> FunctionDefinition
transitionFunction (StateMachine smName) e ss =
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
        tType = case e of EventEnter -> "enter"; EventExit -> "exit"
        f_name = smMangledName ++ "_" ++ tType
        state_var = (#:) (smMangledName ++ "_state") (:#)
        ssMangled = [smMangledName ++ "_" ++ (mangleIdentifier s) | (State s) <- ss]
        case_stmt s = let state_case = (#:) s (:#)
                          state_evt_handler = (#:) (s ++ "_" ++ tType) (:#)
                          call_state_evt_handler = (#:) (apply state_evt_handler []) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

initializeFunction :: StateMachine -> FunctionDefinition
initializeFunction (StateMachine smName) =
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
                                            (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_enter]) SEMICOLON,
                                                              EStatement $ ExpressionStatement (Just $ fromList [init_set]) SEMICOLON])
                                       RIGHTCURLY)
                                       Nothing])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        f_name = smMangledName ++ "_initialize"
        init_var = "initialized"
        init_init = "INITIALIZED"
        init_uninit = "UNINITIALIZED"
        enter_f = (#:) (smMangledName ++ "_enter") (:#)
        call_enter = (#:) (apply enter_f []) (:#)
        init_check = (#:) ((#:) init_init (:#) `NOTEQUAL` (#:) init_var (:#)) (:#)
        init_set = (#:) init_var (:#) `ASSIGN` (#:) init_init (:#)

handleStateEventFunction :: StateMachine -> State -> Happening -> State -> FunctionDefinition
handleStateEventFunction (StateMachine smName) (State s) h (State s') =
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
        sMangledName = mangleIdentifier s
        destStateMangledName = mangleIdentifier s'
        evMangledName = case event h of
                            (Event evName) -> mangleIdentifier evName
                            (EventEnter) -> "enter"
                            (EventExit) -> "exit"
                            (EventAny) -> "any"
        hasPs = case event h of
                        (Event _) -> True
                        otherwise -> False
        f_name = smMangledName ++ "_" ++ sMangledName ++ "_" ++ evMangledName
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"
        event_var = "e"
        event_ex = (#:) event_var (:#)
        dest_state = (#:) (smMangledName ++ "_" ++ destStateMangledName) (:#)
        state_var = (#:) (smMangledName ++ "_state") (:#)
        assign_state = (state_var `ASSIGN` dest_state)
        exit_f = (#:) (smMangledName ++ "_exit") (:#)
        enter_f = (#:) (smMangledName ++ "_enter") (:#)
        call_exit = (#:) (apply exit_f []) (:#)
        call_enter = (#:) (apply enter_f []) (:#)
        side_effects = case h of
                          (Happening _ ses [])                        -> [(#:) (apply ((#:) se (:#)) (if hasPs then [event_ex] else [])) (:#) | (FuncVoid se) <- ses] ++ [call_exit, assign_state, call_enter]
                          (Happening _ ses fs) | elem NoTransition fs -> [(#:) (apply ((#:) se (:#)) (if hasPs then [event_ex] else [])) (:#) | (FuncVoid se) <- ses]

handleEventFunction :: Bool -> StateMachine -> Event -> [State] -> [State] -> [State] -> FunctionDefinition
handleEventFunction debug (StateMachine smName) (Event evName) ss anys unss =
    makeFunction (fromList [B VOID]) [] f_name [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                            (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
    (CompoundStatement
    LEFTCURLY
      (if not debug then Nothing else
        (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator name_var)
                                                        (Just $ Pair EQUAL $ AInitializer evname_e)])
                                      SEMICOLON]))
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
        ssMangled = [smMangledName ++ "_" ++ (mangleIdentifier s) | (State s) <- ss]
        anysMangled = [extendMangledIdentifier smMangledName [s] | (State s) <- anys]
        unssMangled = [smMangledName ++ "_" ++ (mangleIdentifier s) | (State s) <- unss]
        evMangledName = mangleIdentifier evName
        f_name = smMangledName ++ "_" ++ evMangledName
        event_type = f_name ++ "_t"
        event_var = "e"
        name_var = "event_name"
        event_ex = (#:) event_var (:#)
        name_ex = (#:) name_var (:#)
        evname_e = (#:) (show evName) (:#)
        state_var = (#:) (smMangledName ++ "_state") (:#)
        unhandled = (#:) (smMangledName ++ "_UNHANDLED_EVENT") (:#)
        initialize = (#:) (smMangledName ++ "_initialize") (:#)
        call_unhandled = (#:) (apply unhandled (if debug then [name_ex] else [])) (:#)
        call_initialize = (#:) (apply initialize []) (:#)
        unhd_stmt s = let state_case = (#:) s (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON
        case_stmt s en es = let state_case = (#:) s (:#)
                                state_evt_handler = (#:) (extendMangledIdentifier s [en]) (:#)
                                call_state_evt_handler = (#:) (apply state_evt_handler es) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

unhandledEventFunction :: Bool -> StateMachine -> FunctionDefinition
unhandledEventFunction debug (StateMachine smName) =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name
                                   (if not debug then [ParameterDeclaration (fromList [B VOID]) Nothing]
                                    else [ParameterDeclaration (fromList [C CONST, B CHAR])
                                          (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)])
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_assert_f]) SEMICOLON])
    RIGHTCURLY)
    where
        event_var = "e"
        smMangledName = mangleIdentifier smName
        f_name = smMangledName ++ "_UNHANDLED_EVENT"
        event_ex = (#:) event_var (:#)
        assert_f = (#:) (if debug then "printf_assert" else "assert") (:#)
        assert_s = (#:) (show (smName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (smMangledName ++ "_State_name") (:#)
        state_var = (#:) (smMangledName ++ "_state") (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_assert_f = (#:) (apply assert_f (if debug then [assert_s, call_sname_f, event_ex] else [])) (:#)

stateNameFunction :: StateMachine -> [State] -> FunctionDefinition
stateNameFunction (StateMachine smName) ss =
    makeFunction (fromList [A STATIC, C CONST, B CHAR]) [POINTER Nothing] f_name
                                           [ParameterDeclaration (fromList [B $ TypeSpecifier smMangledName])
                                            (Just $ Left $ Declarator Nothing $ IDirectDeclarator state_var)] 
    (CompoundStatement
    LEFTCURLY
        (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER Nothing]) $
                                                                        CDirectDeclarator (IDirectDeclarator names_var) LEFTSQUARE Nothing RIGHTSQUARE)
                                                        (Just $ Pair EQUAL (LInitializer LEFTCURLY
                                                                                         (fromList [AInitializer ((#:) (show s) (:#)) | (State s) <- ss])
                                                                                         Nothing
                                                                                         RIGHTCURLY))])
                                      SEMICOLON,
                          Declaration (fromList [C CONST, B INT])
                                      (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator count_var))
                                                        (Just $ Pair EQUAL (AInitializer names_count_e ))])
                                      SEMICOLON])
        (Just $ fromList [JStatement $ RETURN (Just $ fromList [safe_array_index_e]) SEMICOLON])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName ++ "_State"
        count_var = "state_count"
        state_var = "s"
        names_var = "state_name"
        f_name = smMangledName ++ "_name"
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
handleStateEventDeclaration (StateMachine smName) (State s) e =
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
        sMangledName = mangleIdentifier s
        evMangledName = case e of
                            (Event evName) -> mangleIdentifier evName
                            (EventEnter) -> "enter"
                            (EventExit) -> "exit"
                            (EventAny) -> "any"
        hasPs = case e of
                        (Event _) -> True
                        otherwise -> False
        f_name = smMangledName ++ "_" ++ sMangledName ++ "_" ++ evMangledName
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"

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
        f_name = smMangledName ++ "_" ++ evMangledName
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"

stateVarDeclaration :: StateMachine -> State -> Declaration
stateVarDeclaration (StateMachine smName) (State s) =
    Declaration
    (fromList [A STATIC, B $ TypeSpecifier smEnum])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator state_var)) 
                                     (Just $ Pair EQUAL $ AInitializer ((#:) sMangled (:#)))])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName
        smEnum = smMangledName ++ "_State"
        sMangled = smMangledName ++ "_" ++ (mangleIdentifier s)
        state_var = smMangledName ++ "_state"

stateEnum :: StateMachine -> [State] -> Declaration
stateEnum (StateMachine smName) ss =
    Declaration
    (fromList [A TYPEDEF,
               B (makeEnum smMangledName ssMangled)])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator smMangledName)) Nothing])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName ++ "_State"
        ssMangled = [(mangleIdentifier smName) ++ "_" ++ (mangleIdentifier s) | (State s) <- ss]

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
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"

makeStruct :: Identifier -> [(SpecifierQualifierList, Identifier)] -> TypeSpecifier
makeStruct smName [] = STRUCT (Left $ smName)
makeStruct smName ss = 
    STRUCT (Right (Quad (Just $ smName)
    LEFTCURLY
    (fromList [StructDeclaration sqs (fromList [StructDeclarator $ This $ Declarator Nothing $ IDirectDeclarator id]) SEMICOLON | (sqs, id) <- ss])
    RIGHTCURLY))

makeFunction :: DeclarationSpecifiers -> [Pointer] -> Identifier -> [ParameterDeclaration] -> CompoundStatement -> FunctionDefinition
makeFunction dss ps f_name params body =
    Function
    (Just dss)
    (Declarator (if null ps then Nothing else Just $ fromList ps)
        $ PDirectDeclarator
          (IDirectDeclarator $ f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList (fromList params) Nothing)
          RIGHTPAREN)
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
    generate os gs inputName = sequence [writeTranslationUnit (renderHdr hdr []) (headerName os),
                                         writeTranslationUnit (renderSrc src [extHdrName os, headerName os]) (outputName os)]
        where src = fromList $ concat tus
              hdr = fromList $ concat tuh
              tuh = [[ExternalDeclaration $ Right $ eventStruct sm e
                         | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Right $ handleEventDeclaration sm e
                         | (e, _) <- toList $ events g]
                     | (sm, g) <- gs]
              tus = [[ExternalDeclaration $ Right $ stateEnum sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration sm $ initial g]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration sm s e
                         | (n, EnterExitState {en, st = s@(State _), ex}) <- labNodes g, e <- (mb2e EventEnter en) ++ (map (event . edgeLabel) $ out g n) ++ (mb2e EventExit ex)]
                     ++ (if debug then [ExternalDeclaration $ Left $ stateNameFunction sm $ states g] else [])
                     ++ [ExternalDeclaration $ Left $ unhandledEventFunction debug sm]
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventEnter
                         $ [st | (_, EnterExitState {en = (_:_), st}) <- labNodes g]
                         ++ [st | (n, EnterExitState {st}) <- labNodes g, (_, _, Happening EventEnter _ _) <- out g n]]
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventExit
                         [st | (_, EnterExitState {st, ex = (_:_)}) <- labNodes g]]
                     ++ [ExternalDeclaration $ Left $ initializeFunction sm]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction debug sm e ss (anys g \\ ss) ((states g \\ ss) \\ (anys g))
                         | (e, ss) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s
                         | (n, EnterExitState {en, st = s@(State _)}) <- labNodes g, h <- mb2h EventEnter en]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s'
                         | (n, EnterExitState {st = s@(State _)}) <- labNodes g, (_, n', h) <- out g n, Just EnterExitState {st = s'@(State _)} <- [lab g n']]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s
                         | (n, EnterExitState {st = s@(State _), ex}) <- labNodes g, h <- mb2h EventExit ex]
                     | (sm, g) <- gs]
              initial g = head [st ese | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n, (Just ese) <- [lab g n']]
              states g = [st ees | (_, ees) <- labNodes g]
              anys g = [st ees | (n, ees) <- labNodes g, (_, _, Happening {event = EventAny}) <- out g n]
              events g = foldl insert_event empty [(h, st ees) | (n, ees) <- labNodes g, (_, _, h) <- out g n]
              insert_event m ((Happening e@(Event _) _ _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m                                          _ = m
              mb2e d me = if null me then [] else [d]
              mb2h d me = if null me then [] else [Happening d me [NoTransition]]
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
