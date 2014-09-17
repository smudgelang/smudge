module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), Happening(..))
import Grammars.C89
import Trashcan.FilePath (relPath)
import Unparsers.C89 (renderPretty)

import Data.Graph.Inductive.Graph (labNodes, lab, out, context, nodes)
import Data.List ((\\))
import Data.Map (insertWith, empty, toList)
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension, takeDirectory, (<.>))

data CStaticOption = OutFile FilePath | Header FilePath | NoDebug
    deriving (Show, Eq)

apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

transitionFunction :: StateMachine -> Event -> [State] -> FunctionDefinition
transitionFunction (StateMachine smName) e ss =
    Function
    (Just $ fromList [A STATIC, B VOID])
    (Declarator Nothing $ PDirectDeclarator (IDirectDeclarator $ f_name) LEFTPAREN Nothing RIGHTPAREN)
    Nothing
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

changeStateFunction :: StateMachine -> FunctionDefinition
changeStateFunction (StateMachine smName) =
    Function
    (Just $ fromList [A STATIC, B VOID])
    (Declarator Nothing
        $ PDirectDeclarator
          (IDirectDeclarator $ f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList
                         (fromList [ParameterDeclaration (fromList [B $ TypeSpecifier smEnum])
                                    (Just $ Left $ Declarator Nothing $ IDirectDeclarator state_param)])
                         Nothing)
          RIGHTPAREN)
    Nothing
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_exit]) SEMICOLON,
                          EStatement $ ExpressionStatement (Just $ fromList [assign_state]) SEMICOLON,
                          EStatement $ ExpressionStatement (Just $ fromList [call_enter]) SEMICOLON])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        smEnum = smMangledName ++ "_State"
        f_name = smMangledName ++ "_change_state"
        state_param = "s"
        state_var = (#:) (smMangledName ++ "_state") (:#)
        dest_state = (#:) state_param (:#)
        assign_state = (state_var `ASSIGN` dest_state)
        exit_f = (#:) (smMangledName ++ "_exit") (:#)
        enter_f = (#:) (smMangledName ++ "_enter") (:#)
        call_exit = (#:) (apply exit_f []) (:#)
        call_enter = (#:) (apply enter_f []) (:#)

handleStateEventFunction :: StateMachine -> State -> Happening -> State -> FunctionDefinition
handleStateEventFunction (StateMachine smName) (State s) h (State s') =
    Function
    (Just $ fromList [A STATIC, B VOID])
    (Declarator Nothing
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (if not hasPs then Nothing else
            (Just $ Left $ ParameterTypeList
                           (fromList [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                      (Just $ Left $ Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator event_var)])
                           Nothing))
          RIGHTPAREN)
    Nothing
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [se]) SEMICOLON | se <- side_effects])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        sMangledName = mangleIdentifier s
        destStateMangledName = mangleIdentifier s'
        evMangledName = case (case h of (Hustle e _) -> e; (Bustle e _) -> e) of
                            (Event evName) -> mangleIdentifier evName
                            (EventEnter) -> "enter"
                            (EventExit) -> "exit"
                            (EventAny) -> "any"
        hasPs = case (case h of (Hustle e _) -> e; (Bustle e _) -> e) of
                        (Event _) -> True
                        otherwise -> False
        f_name = smMangledName ++ "_" ++ sMangledName ++ "_" ++ evMangledName
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"
        event_var = "e"
        event_ex = (#:) event_var (:#)
        change_state = (#:) (smMangledName ++ "_change_state") (:#)
        dest_state = (#:) (smMangledName ++ "_" ++ destStateMangledName) (:#)
        call_change = (#:) (apply change_state [dest_state]) (:#)
        side_effects = case h of
                          (Hustle _ ses) -> [(#:) (apply ((#:) se (:#)) (if hasPs then [event_ex] else [])) (:#) | (FuncVoid se) <- ses] ++ [call_change]
                          (Bustle _ ses) -> [(#:) (apply ((#:) se (:#)) (if hasPs then [event_ex] else [])) (:#) | (FuncVoid se) <- ses]

handleEventFunction :: Bool -> StateMachine -> Event -> [State] -> [State] -> FunctionDefinition
handleEventFunction debug (StateMachine smName) (Event evName) ss unss =
    Function
    (Just $ fromList [B VOID])
    (Declarator Nothing
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList
                         (fromList [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                    (Just $ Left $ Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator event_var)])
                         Nothing)
          RIGHTPAREN)
    Nothing
    (CompoundStatement
    LEFTCURLY
      (if not debug then Nothing else
        (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator name_var)
                                                        (Just $ Pair EQUAL $ AInitializer evname_e)])
                                      SEMICOLON]))
        (Just $ fromList [SStatement $ SWITCH LEFTPAREN (fromList [state_var]) RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
                                       Nothing
                                       (Just $ fromList $ concat ([[LStatement $ case_stmt s, JStatement $ BREAK SEMICOLON] | s <- ssMangled]
                                                                 ++ [[LStatement $ unhd_stmt s, JStatement $ BREAK SEMICOLON] | s <- unssMangled])
                                                                 ++ [LStatement $ DEFAULT COLON $
                                                                     EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON,
                                                                     JStatement $ BREAK SEMICOLON])
                                       RIGHTCURLY])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        ssMangled = [smMangledName ++ "_" ++ (mangleIdentifier s) | (State s) <- ss]
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
        call_unhandled = (#:) (apply unhandled (if debug then [name_ex] else [])) (:#)
        unhd_stmt s = let state_case = (#:) s (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON
        case_stmt s = let state_case = (#:) s (:#)
                          state_evt_handler = (#:) (s ++ "_" ++ evMangledName) (:#)
                          call_state_evt_handler = (#:) (apply state_evt_handler [event_ex]) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

unhandledEventFunction :: Bool -> StateMachine -> FunctionDefinition
unhandledEventFunction debug (StateMachine smName) =
    Function
    (Just $ fromList [A STATIC, B VOID])
    (Declarator Nothing
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (if not debug then Nothing else
            (Just $ Left $ ParameterTypeList
                         (fromList [ParameterDeclaration (fromList [C CONST, B CHAR])
                                    (Just $ Left $ Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator event_var)])
                         Nothing))
          RIGHTPAREN)
    Nothing
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
        assert_s = (#:) (show (smMangledName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (smMangledName ++ "_State_name") (:#)
        state_var = (#:) (smMangledName ++ "_state") (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_assert_f = (#:) (apply assert_f (if debug then [assert_s, call_sname_f, event_ex] else [])) (:#)

stateNameFunction :: StateMachine -> [State] -> FunctionDefinition
stateNameFunction (StateMachine smName) ss =
    Function
    (Just $ fromList [A STATIC, C CONST, B CHAR])
    (Declarator (Just $ POINTER Nothing Nothing)
                $ PDirectDeclarator
                  (IDirectDeclarator $ smMangledName ++ "_name")
                  LEFTPAREN
                  (Just $ Left $ ParameterTypeList
                                 (fromList [ParameterDeclaration (fromList [B $ TypeSpecifier smMangledName])
                                            (Just $ Left $ Declarator Nothing $ IDirectDeclarator state_var)])
                                 Nothing)
                  RIGHTPAREN)
    Nothing
    (CompoundStatement
    LEFTCURLY
        (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ POINTER Nothing Nothing) $
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
        names_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier names_var]) Nothing) RIGHTPAREN) (:#)
        ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                     (Just $ AbstractDeclarator $ This $ POINTER Nothing Nothing)) RIGHTPAREN) (:#)
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
          (if not hasPs then Nothing else
            (Just $ Left $ ParameterTypeList
                           (fromList [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                      (Just $ Right $ AbstractDeclarator $ This $ POINTER Nothing Nothing)])
                           Nothing))
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
                                    (Just $ Right $ AbstractDeclarator $ This $ POINTER Nothing Nothing)])
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
    ENUM (Right (Quad (Just $ smName)
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

instance Backend CStaticOption where
    options = ("c",
               [Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["h"] (ReqArg Header "FILE")
                 "The name of the target header file if not derived from source file.",
                Option [] ["no-debug"] (NoArg NoDebug)
                 "Don't generate debugging information"])
    generate os gs inputName = sequence [writeTranslationUnit hdr $ headerName os,
                                         writeTranslationUnit src $ outputName os]
        where src = fromList $ concat tus
              hdr = fromList $ concat tuh
              tuh = [[ExternalDeclaration $ Right $ eventStruct sm e
                         | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Right $ handleEventDeclaration sm e
                         | (e, _) <- toList $ events g]
                     | (sm, g) <- gs]
              tus = [[ExternalDeclaration $ Right $ stateEnum sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration sm $ [s | s@(State _) <- (states g)] !! 0]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration sm s e
                         | (n, (en, s@(State _), ex)) <- labNodes g, e <- (mb2e EventEnter en) ++ (map (eventOf . edgeLabel) $ out g n) ++ (mb2e EventExit ex)]
                     ++ (if debug then [ExternalDeclaration $ Left $ stateNameFunction sm $ states g] else [])
                     ++ [ExternalDeclaration $ Left $ unhandledEventFunction debug sm]
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventEnter
                         [s | (_, (Just (e:_), s, _)) <- labNodes g]]
                     ++ [ExternalDeclaration $ Left $ transitionFunction sm EventExit
                         [s | (_, (_, s, Just (e:_))) <- labNodes g]]
                     ++ [ExternalDeclaration $ Left $ changeStateFunction sm]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction debug sm e ss (states g \\ ss)
                         | (e, ss) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s
                         | (n, (en, s@(State _), ex)) <- labNodes g, h <- mb2h EventEnter en]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s'
                         | (n, (_, s@(State _), _)) <- labNodes g, (_, n', h) <- out g n, (Just (_, s'@(State _), _)) <- [lab g n']]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s h s
                         | (n, (en, s@(State _), ex)) <- labNodes g, h <- mb2h EventExit ex]
                     | (sm, g) <- gs]
              states g = [s | (_, (_, s, _)) <- labNodes g]
              events g = foldl insert_event empty [(h, s) | (n, (_, s, _)) <- labNodes g, (_, _, h) <- out g n]
              insert_event m ((Hustle e@(Event _) _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m ((Bustle e@(Event _) _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m                                     _ = m
              mb2e d me = maybe [] (\_ -> [d]) me
              mb2h d me = maybe [] (\ss -> [Bustle d ss]) me
              edgeLabel (_, _, l) = l
              eventOf (Hustle e _) = e
              eventOf (Bustle e _) = e
              writeTranslationUnit u fp = (writeFile fp (renderPretty u)) >> (return fp)
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName ((dropExtension inputName) <.> "c") xs
              headerName ((Header f):_) = f
              headerName xs = getFirstOrDefault headerName ((dropExtension inputName) <.> "h") xs
              headerIncludePath = relPath (takeDirectory $ outputName os) (headerName os)
              doDebug ((NoDebug):_) = False
              doDebug xs = getFirstOrDefault doDebug True xs
              debug = doDebug os
