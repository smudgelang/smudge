module Backends.CStatic where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), Happening(..))
import Grammars.C89
import Unparsers.C89 (renderPretty)

import Data.Graph.Inductive.Graph (labNodes, out)
import Data.Map (insertWith, empty, toList)
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension, (<.>))

data CStaticOption = OutFile FilePath
    deriving (Show, Eq)

apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

handleStateEventFunction :: StateMachine -> State -> Event -> FunctionDefinition
handleStateEventFunction (StateMachine smName) (State s) (Event evName) =
    Function
    (Just $ fromList [A STATIC, B VOID])
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
        Nothing
        Nothing
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        sMangledName = mangleIdentifier s
        evMangledName = mangleIdentifier evName
        f_name = smMangledName ++ "_" ++ sMangledName ++ "_" ++ evMangledName
        event_type = smMangledName ++ "_" ++ evMangledName ++ "_t"
        event_var = "e"

handleEventFunction :: StateMachine -> Event -> [State] -> FunctionDefinition
handleEventFunction (StateMachine smName) (Event evName) ss =
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
        (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                      (Just $ fromList [InitDeclarator (Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator name_var)
                                                        (Just $ Pair EQUAL $ AInitializer evname_e)])
                                      SEMICOLON])
        (Just $ fromList [SStatement $ SWITCH LEFTPAREN (fromList [state_var]) RIGHTPAREN $ CStatement $ CompoundStatement LEFTCURLY
                                       Nothing
                                       (Just $ fromList $ concat [[LStatement $ case_stmt s, JStatement $ BREAK SEMICOLON] | s <- ssMangled]
                                                                 ++ [LStatement $ DEFAULT COLON $
                                                                     EStatement $ ExpressionStatement (Just $ fromList [call_unhandled]) SEMICOLON,
                                                                     JStatement $ BREAK SEMICOLON])
                                       RIGHTCURLY])
    RIGHTCURLY)
    where
        smMangledName = mangleIdentifier smName
        ssMangled = [smMangledName ++ "_" ++ (mangleIdentifier s) | (State s) <- ss]
        evMangledName = mangleIdentifier evName
        f_name = smMangledName ++ "_" ++ evMangledName
        event_type = f_name ++ "_t"
        event_var = "e"
        name_var = "event_name"
        event_ex = (#:) event_var (:#)
        name_ex = (#:) name_var (:#)
        evname_e = (#:) (show evName) (:#)
        state_var = (#:) "state" (:#)
        unhandled = (#:) (smMangledName ++ "_UNHANDLED_EVENT") (:#)
        call_unhandled = (#:) (apply unhandled [name_ex]) (:#)
        case_stmt s = let state_case = (#:) s (:#)
                          state_evt_handler = (#:) (s ++ "_" ++ evMangledName) (:#)
                          call_state_evt_handler = (#:) (apply state_evt_handler [event_ex]) (:#) in
                        CASE state_case COLON $
                        EStatement $ ExpressionStatement (Just $ fromList [call_state_evt_handler]) SEMICOLON

unhandledEventFunction :: StateMachine -> FunctionDefinition
unhandledEventFunction (StateMachine smName) =
    Function
    (Just $ fromList [A STATIC, B VOID])
    (Declarator Nothing
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList
                         (fromList [ParameterDeclaration (fromList [C CONST, B CHAR])
                                    (Just $ Left $ Declarator (Just $ POINTER Nothing Nothing) $ IDirectDeclarator event_var)])
                         Nothing)
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
        assert_f = (#:) "printf_assert" (:#)
        assert_s = (#:) (show (smMangledName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (smMangledName ++ "_State_name") (:#)
        state_var = (#:) "state" (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_assert_f = (#:) (apply assert_f [assert_s, call_sname_f, event_ex]) (:#)

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
handleStateEventDeclaration (StateMachine smName) (State s) (Event evName) =
    Declaration
    (fromList [A STATIC, B VOID])
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
        sMangledName = mangleIdentifier s
        evMangledName = mangleIdentifier evName
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
    (fromList [A STATIC, B $ TypeSpecifier smMangledName])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator state_var)) 
                                     (Just $ Pair EQUAL $ AInitializer ((#:) sMangled (:#)))])
    SEMICOLON
    where
        smMangledName = mangleIdentifier smName ++ "_State"
        sMangled = (mangleIdentifier smName) ++ "_" ++ (mangleIdentifier s)
        state_var = "state"

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
                 "The name of the target file if not derived from source file."])
    generate os gs inputName = writeTranslationUnit tu (outputName os)
        where tu = fromList $ concat tus
              tus = [[ExternalDeclaration $ Right $ eventStruct sm e
                         | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Right $ handleEventDeclaration sm e
                         | (e, _) <- toList $ events g]
                     ++ [ExternalDeclaration $ Right $ stateEnum sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration sm $ [s | s@(State _) <- (states g)] !! 0]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration sm s e
                         | (n, s@(State _)) <- labNodes g, e@(Event _) <- map (eventOf . edgeLabel) $ out g n]
                     ++ [ExternalDeclaration $ Left $ stateNameFunction sm $ states g,
                         ExternalDeclaration $ Left $ unhandledEventFunction sm]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction sm e ss
                         | (e, ss) <- toList $ events g]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction sm s e
                         | (n, s@(State _)) <- labNodes g, e@(Event _) <- map (\ (_, _, h) -> eventOf h) $ out g n]
                     | (sm, g) <- gs]
              states g = [s | (_, s) <- labNodes g]
              events g = foldl insert_event empty [(h, s) | (n, s) <- labNodes g, (_, _, h) <- out g n]
              insert_event m ((Hustle e@(Event _) _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m ((Bustle e@(Event _) _), s@(State _)) = insertWith (flip (++)) e [s] m
              insert_event m                                     _ = m
              edgeLabel (_, _, l) = l
              eventOf (Hustle e _) = e
              eventOf (Bustle e _) = e
              writeTranslationUnit u fp = (writeFile fp (renderPretty u)) >> (return fp)
              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName ((dropExtension inputName) <.> "c") xs
