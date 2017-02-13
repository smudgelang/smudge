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
  QualifiedName,
  Qualifiable(qualify),
  TaggedName(..),
  extractWith,
  mangleWith,
  disqualifyTag,
  qName,
  )
import Semantics.Operation (handlers, finalStates)
import Semantics.Solver (
  Ty(..), 
  Binding(..), 
  resultOf,
  SymbolTable, 
  insertFunctions,
  (!),
  )
import qualified Semantics.Solver as Solver(toList)
import Semantics.Alias (Alias, rename)
import Trashcan.FilePath (relPath)
import Trashcan.These (
  These(..),
  maybeThis,
  maybeThat,
  theseAndThat,
  fmapThat,
  )
import Unparsers.C89 (renderPretty)

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Graph.Inductive.Graph (labNodes, labEdges, lab, out, suc, insEdges, nodes, delNodes)
import Data.List (intercalate, nub, sort, (\\), dropWhileEnd)
import Data.Map (empty, toList)
import qualified Data.Map (null, (!))
import Data.Text (replace)
import System.Console.GetOpt
import System.IO.Unsafe (unsafePerformIO)
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

apply :: PostfixExpression -> [AssignmentExpression] -> PostfixExpression
apply f [] = APostfixExpression f LEFTPAREN Nothing RIGHTPAREN
apply f ps = APostfixExpression f LEFTPAREN (Just $ fromList ps) RIGHTPAREN

(+-+) :: Identifier -> Identifier -> Identifier
a +-+ b = dropWhileEnd (== '_') a ++ "_" ++ dropWhile (== '_') b

qualifyMangle :: Qualifiable q => Alias QualifiedName -> q -> Identifier
qualifyMangle aliases q = mangleWith (+-+) mangleIdentifier $ rename aliases $ qualify q

mangleTName :: Alias QualifiedName -> TaggedName -> Identifier
mangleTName aliases (TagEvent q) = qualifyMangle aliases q +-+ "t"
mangleTName aliases t = qualifyMangle aliases t

mangleEv :: Event TaggedName -> QualifiedName
mangleEv (Event evName) = extractWith seq qualify $ qualify evName
mangleEv EventEnter = qualify "enter"
mangleEv EventExit = qualify "exit"
mangleEv EventAny = qualify "any"

transitionFunction :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> Event TaggedName -> [State TaggedName] -> FunctionDefinition
transitionFunction aliases (StateMachineDeclarator smName) e@EventExit ss =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name params
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [makeSwitch (fromList [state_var]) cases []])
    RIGHTCURLY)
    where
        evAlone = mangleEv e
        f_name = qualifyMangle aliases (sName smName StateAny, evAlone)
        params = case e of
                    otherwise -> [ParameterDeclaration (fromList [B VOID]) Nothing]
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        call_ev_in s n vs = (#:) (apply ((#:) (qualifyMangle aliases (s, n)) (:#)) vs) (:#)
        cases = [((#:) (mangleTName aliases s) (:#), [fromList [call_ev_in s evAlone []]]) | (State s) <- ss]

initializeFunction :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> State TaggedName -> FunctionDefinition
initializeFunction aliases (StateMachineDeclarator smName) (State s) =
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
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        f_name = qualifyMangle aliases (smName, "initialize")
        init_var = "initialized"
        init_init = "INITIALIZED"
        init_uninit = "UNINITIALIZED"
        assign_state = state_var `ASSIGN` ((#:) (mangleTName aliases s) (:#))
        enter_f = (#:) (qualifyMangle aliases (s, "enter")) (:#)
        call_enter = (#:) (apply enter_f []) (:#)
        init_check = (#:) ((#:) init_init (:#) `NOTEQUAL` (#:) init_var (:#)) (:#)
        init_set = (#:) init_var (:#) `ASSIGN` (#:) init_init (:#)
        side_effects = [assign_state, call_enter, init_set]

sName :: TaggedName -> State TaggedName -> QualifiedName
sName _ (State s) = qualify s
sName smName StateAny  = qualify (smName, "ANY_STATE")

handleStateEventFunction :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> State TaggedName -> Happening -> State TaggedName -> Bool -> SymbolTable -> FunctionDefinition
handleStateEventFunction aliases sm@(StateMachineDeclarator smName) st h st' do_exit syms =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name params
    (CompoundStatement
    LEFTCURLY
        Nothing
        (if null side_effects then Nothing else Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [se]) SEMICOLON | se <- side_effects])
    RIGHTCURLY)
    where
        destStateMangledName = qualifyMangle aliases $ sName smName st'
        params = case event h of
                    (Event t) -> [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier $ mangleTName aliases t])
                                  (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
                    otherwise -> [ParameterDeclaration (fromList [B VOID]) Nothing]
        f_name = qualifyMangle aliases (sName smName st, mangleEv $ event h)
        event_var = "e"
        event_ex = (#:) event_var (:#)
        dest_state = (#:) destStateMangledName (:#)
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        assign_state = (state_var `ASSIGN` dest_state)
        exit_f = (#:) (qualifyMangle aliases (sName smName st, "exit")) (:#)
        enter_f = (#:) (qualifyMangle aliases (sName smName st', "enter")) (:#)
        call_exit = (#:) (apply exit_f []) (:#)
        call_enter = (#:) (apply enter_f []) (:#)

        isEventTy :: Ty -> Event TaggedName -> Bool
        isEventTy a (Event e) = a == snd (syms ! e)
        isEventTy _ _ = False

        psOf (Void :-> _) = []
        psOf (p    :-> _) = [if isEventTy p (event h) then event_ex else (#:) "0" (:#)]
        apply_se (f, FuncTyped _) = undefined -- See ticket #15, harder than it seems at first.
        apply_se (f, _) = (#:) (apply ((#:) (mangleTName aliases f) (:#)) (psOf (snd $ syms ! f))) (:#)
        side_effects = case h of
                          (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ (if do_exit then [call_exit] else []) ++ [assign_state, call_enter]
                          (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

handleEventFunction :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> Event TaggedName -> [(State TaggedName, (State TaggedName, Event TaggedName))] -> [State TaggedName] -> FunctionDefinition
handleEventFunction aliases (StateMachineDeclarator smName) e@(Event evName) ss unss =
    makeFunction (fromList [B VOID]) [] f_name params
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_initialize]) SEMICOLON,
                          makeSwitch (fromList [state_var]) cases defaults])
    RIGHTCURLY)
    where
        evAlone = mangleEv e
        f_name = qualifyMangle aliases evName
        params = case e of
                    (Event t) -> [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier $ mangleTName aliases t])
                                  (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
        event_var = "e"
        event_ex = (#:) event_var (:#)
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        unhandled = (#:) (qualifyMangle aliases (qualify (smName, "UNHANDLED_EVENT"), evAlone)) (:#)
        initialize = (#:) (qualifyMangle aliases (smName, "initialize")) (:#)
        call_unhandled = (#:) (apply unhandled [event_ex]) (:#)
        call_initialize = (#:) (apply initialize []) (:#)
        call_ev_in s ev vs = (#:) (apply ((#:) (qualifyMangle aliases (s, mangleEv ev)) (:#)) vs) (:#)
        esOf EventAny = []
        esOf e' | e == e' = [(#:) event_var (:#)]
        cases = [((#:) (mangleTName aliases s) (:#), [fromList [call_ev_in s' ev (esOf ev)]]) | (State s, (State s', ev)) <- ss]
             ++ [((#:) (mangleTName aliases s) (:#), [fromList [call_unhandled]]) | (State s) <- unss]
        defaults = [fromList [call_unhandled]]

unhandledEventFunction :: Alias QualifiedName -> Bool -> [(State TaggedName, Event TaggedName)] -> StateMachineDeclarator TaggedName -> Event TaggedName -> FunctionDefinition
unhandledEventFunction aliases debug handler (StateMachineDeclarator smName) e@(Event evName) =
    makeFunction (fromList [A STATIC, B VOID]) [] f_name [ParameterDeclaration (fromList [C CONST, B $ TypeSpecifier event_type])
                                                          (Just $ Left $ Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator event_var)]
    (CompoundStatement
    LEFTCURLY
        (if (not $ null handler) || not debug then Nothing else
          (Just $ fromList [Declaration (fromList [C CONST, B CHAR]) 
                                        (Just $ fromList [InitDeclarator (Declarator (Just $ fromList [POINTER Nothing]) $ IDirectDeclarator name_var)
                                                          (Just $ Pair EQUAL $ AInitializer evname_e)])
                                        SEMICOLON]))
        (Just $ fromList [EStatement $ ExpressionStatement (Just $ fromList [call_handler_f]) SEMICOLON])
    RIGHTCURLY)
    where
        name_var = "event_name"
        name_ex = (#:) name_var (:#)
        evAlone = mangleEv e
        evname_e = (#:) (show $ disqualifyTag evName) (:#)
        f_name = qualifyMangle aliases (qualify (smName, "UNHANDLED_EVENT"), evAlone)
        event_type = mangleTName aliases evName
        event_var = "e"
        panic_f = (#:) (qualifyMangle aliases $ if debug then "panic_print" else "panic") (:#)
        panic_s = (#:) (show (disqualifyTag smName ++ "[%s]: Unhandled event \"%s\"\n")) (:#)
        sname_f  = (#:) (qualifyMangle aliases (smName, "State_name")) (:#)
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)
        call_panic_f = (#:) (apply panic_f (if debug then [panic_s, call_sname_f, name_ex] else [])) (:#)
        handle_f s e = (#:) (qualifyMangle aliases (sName smName s, mangleEv e)) (:#)
        esOf EventAny = []
        esOf e' | e == e' = [(#:) event_var (:#)]
        call_handler_f = case handler of
                         [(s, e)] -> (#:) (apply (handle_f s e) (esOf e)) (:#)
                         [] -> call_panic_f

stateNameFunction :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> [State TaggedName] -> FunctionDefinition
stateNameFunction aliases (StateMachineDeclarator smName) ss =
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
        smEnum = qualifyMangle aliases (smName, "State")
        count_var = "state_count"
        state_var = "s"
        names_var = "state_name"
        f_name = qualifyMangle aliases (smName, "State_name")
        names_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Left $ TypeSpecifier names_var]) Nothing) RIGHTPAREN) (:#)
        ptr_size_e = (#:) (SIZEOF $ Right $ Trio LEFTPAREN (TypeName (fromList [Right CONST, Left CHAR])
                                                                     (Just $ This $ fromList [POINTER Nothing])) RIGHTPAREN) (:#)
        names_count_e = (#:) (names_size_e `DIV` ptr_size_e) (:#)
        count_var_e = (#:) count_var (:#)
        state_var_e = (#:) state_var (:#)
        names_var_e = (#:) names_var (:#)
        default_state = (#:) (show "INVALID_STATE") (:#)
        bounds_check_e = (#:) (state_var_e `LESS_THAN` count_var_e) (:#)
        array_index_e = (#:) (EPostfixExpression names_var_e LEFTSQUARE (fromList [(#:) state_var_e (:#)]) RIGHTSQUARE) (:#)
        safe_array_index_e = (#:) (bounds_check_e `QUESTION` (Trio (fromList [array_index_e]) COLON default_state)) (:#)

currentStateNameFunction :: Alias QualifiedName -> Bool -> StateMachineDeclarator TaggedName -> FunctionDefinition
currentStateNameFunction aliases debug (StateMachineDeclarator smName) = 
    makeFunction (fromList [C CONST, B CHAR]) [POINTER Nothing] f_name [ParameterDeclaration (fromList [B VOID]) Nothing]
    (CompoundStatement
    LEFTCURLY
        Nothing
        (Just $ fromList [JStatement $ RETURN (Just $ fromList [if debug then call_sname_f else ((#:) (show "") (:#))]) SEMICOLON])
    RIGHTCURLY)
    where
        f_name = qualifyMangle aliases (smName, "Current_state_name")
        sname_f  = (#:) (qualifyMangle aliases (smName, "State_name")) (:#)
        state_var = (#:) (qualifyMangle aliases (smName, "state")) (:#)
        call_sname_f = (#:) (apply sname_f [state_var]) (:#)

handleStateEventDeclaration :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> State TaggedName -> Event TaggedName -> Declaration
handleStateEventDeclaration aliases (StateMachineDeclarator smName) st e =
    Declaration spec (Just $ fromList [InitDeclarator declr Nothing]) SEMICOLON
    where
        (dspec, declr) = declare aliases (qualify (sName smName st, mangleEv e)) ty
        spec = SimpleList (A STATIC) (Just dspec)
        ty = case e of
                    (Event t) -> Ty t :-> Void
                    otherwise -> Void :-> Void

stateVarDeclaration :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> State TaggedName -> Declaration
stateVarDeclaration aliases (StateMachineDeclarator smName) (State s) =
    Declaration
    (fromList [A STATIC, B $ TypeSpecifier smEnum])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator state_var)) 
                                     (Just $ Pair EQUAL $ AInitializer ((#:) sMangled (:#)))])
    SEMICOLON
    where
        smEnum = qualifyMangle aliases (smName, "State")
        sMangled = mangleTName aliases s
        state_var = qualifyMangle aliases (smName, "state")

stateEnum :: Alias QualifiedName -> StateMachineDeclarator TaggedName -> [State TaggedName] -> Declaration
stateEnum aliases (StateMachineDeclarator smName) ss =
    Declaration
    (fromList [A TYPEDEF,
               B (makeEnum smEnum ssMangled)])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator smEnum)) Nothing])
    SEMICOLON
    where
        smEnum = qualifyMangle aliases (smName, "State")
        ssMangled = [mangleTName aliases s | (State s) <- ss]

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

eventStruct :: Alias QualifiedName -> TaggedName -> Ty -> Declaration
eventStruct aliases name (Ty ty) =
    Declaration
    (fromList [A TYPEDEF,
               B (makeStruct (mangleTName aliases ty) [])])
    (Just $ fromList [InitDeclarator (Declarator Nothing (IDirectDeclarator $ mangleTName aliases name)) Nothing])
    SEMICOLON

makeStruct :: Identifier -> [(SpecifierQualifierList, Identifier)] -> TypeSpecifier
makeStruct name [] = STRUCT (Left $ name)
makeStruct name ss = 
    STRUCT (Right (Quad (Just $ name)
    LEFTCURLY
    (fromList [StructDeclaration sqs (fromList [StructDeclarator $ This $ Declarator Nothing $ IDirectDeclarator id]) SEMICOLON | (sqs, id) <- ss])
    RIGHTCURLY))

makeFunctionDeclaration :: Alias QualifiedName -> TaggedName -> (Binding, Ty) -> Declaration
makeFunctionDeclaration aliases n (b, ty) =
    Declaration spec (Just $ fromList [InitDeclarator declr Nothing]) SEMICOLON
    where
        (dspec, declr) = declare aliases (qualify n) ty
        spec = case b of External -> SimpleList (A EXTERN) (Just dspec); _ -> dspec

makeFunction :: DeclarationSpecifiers -> [Pointer] -> Identifier -> [ParameterDeclaration] -> CompoundStatement -> FunctionDefinition
makeFunction dss ps f_name params body =
    Function
    (Just dss)
    (Declarator (if null ps then Nothing else Just $ fromList ps)
        $ PDirectDeclarator
          (IDirectDeclarator f_name)
          LEFTPAREN
          (Just $ Left $ ParameterTypeList (fromList params) Nothing)
          RIGHTPAREN)
    Nothing
    body

declare :: Alias QualifiedName -> QualifiedName -> Ty -> (DeclarationSpecifiers, Declarator)
declare aliases = convertDeclarator
    where
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
                  absdec = fmapThat pdeclr $ maybe (That Nothing) (theseAndThat Nothing Just) rabsdec
                  toParam = uncurry ParameterDeclaration . second (fmap Right) . convertAbsDeclarator
                  convertF (p :-> r) = second (toParam p :) $ convertF r
                  convertF        r  = (convertAbsDeclarator r, [])

        convertDeclarator :: QualifiedName -> Ty -> (DeclarationSpecifiers, Declarator)
        convertDeclarator x = second inst . convertAbsDeclarator
            where maybename = maybe (IDirectDeclarator $ convertQName x) instdir
                  inst ad = Declarator (ad >>= maybeThis) (maybename $ ad >>= maybeThat)
                  instdir (ADirectAbstractDeclarator LEFTPAREN a RIGHTPAREN)     = DDirectDeclarator LEFTPAREN (inst $ Just a) RIGHTPAREN
                  instdir (CDirectAbstractDeclarator a LEFTSQUARE c RIGHTSQUARE) = CDirectDeclarator (maybename a) LEFTSQUARE c RIGHTSQUARE
                  instdir (PDirectAbstractDeclarator a LEFTPAREN p RIGHTPAREN)   = PDirectDeclarator (maybename a) LEFTPAREN (fmap Left p) RIGHTPAREN

        convertQName = qualifyMangle aliases
        convertTName = mangleTName aliases

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
        where src = fromList $ concat tus
              ext = fromList tue
              hdr = fromList tuh
              tuh = [ExternalDeclaration $ Right $ eventStruct aliases name ty | (name, (Resolved, ty@(Ty _))) <- Solver.toList syms]
                    ++ [ExternalDeclaration $ Right $ makeFunctionDeclaration aliases name f | (name, f@(Resolved, _ :-> _)) <- Solver.toList syms]
                    ++ [ExternalDeclaration $ Right $ makeFunctionDeclaration aliases name f | (name, f) <- Solver.toList externs]
              tue = [ExternalDeclaration $ Right $ makeFunctionDeclaration aliases name f | (name, f@(External, _ :-> _)) <- Solver.toList syms]
              tus = [[ExternalDeclaration $ Right $ stateEnum aliases sm $ states g]
                     ++ [ExternalDeclaration $ Right $ stateVarDeclaration aliases sm $ initial g]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration aliases sm s EventExit | (_, EnterExitState {st = s@StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Right $ handleStateEventDeclaration aliases sm s e
                         | (n, EnterExitState {st = s}) <- labNodes g, e <- (map (event . edgeLabel) $ out g n), case s of State _ -> True; StateAny -> True; _ -> False]
                     ++ (if debug then [ExternalDeclaration $ Left $ stateNameFunction aliases sm $ states g] else [])
                     ++ [ExternalDeclaration $ Left $ currentStateNameFunction aliases debug sm]
                     ++ [ExternalDeclaration $ Left $ transitionFunction aliases sm EventExit
                         [st | (n, EnterExitState {st, ex = (_:_)}) <- labNodes g, n `notElem` finalStates g] | (_, EnterExitState {st = StateAny}) <- labNodes g]
                     ++ [ExternalDeclaration $ Left $ unhandledEventFunction aliases debug (any_handler e g) sm e | e <- events g]
                     ++ [ExternalDeclaration $ Left $ initializeFunction aliases sm $ initial g]
                     ++ [ExternalDeclaration $ Left $ handleEventFunction aliases sm e (s_handlers e g) (unhandled e g) | e <- events g]
                     ++ [ExternalDeclaration $ Left $ handleStateEventFunction aliases sm s h s' (s == StateAny || not (null ex)) syms
                         | (n, EnterExitState {st = s, ex}) <- labNodes g, (_, n', h) <- out g n, Just EnterExitState {st = s'} <- [lab g n'], case s of State _ -> True; StateAny -> True; _ -> False, case s' of State _ -> True; StateAny -> True; _ -> False]
                     | (sm, g) <- gs'']
              gs'' = [(sm, insEdges [(n, n, Happening EventEnter en [NoTransition])
                                     | (n, EnterExitState {en, st = State _}) <- labNodes $ delNodes [n | n <- nodes g, (_, _, Happening EventEnter _ _) <- out g n] g] g)
                      | (sm, g) <- gs']
              gs'  = [(sm, insEdges [(n, n, Happening EventExit ex [NoTransition])
                                     | (n, EnterExitState {st = State _, ex = ex@(_:_)}) <- labNodes $ delNodes (finalStates g ++ [n | n <- nodes g, (_, _, Happening EventExit _ _) <- out g n]) g] g)
                      | (sm, g) <- gs]
              gs = [(smd, g) | (Annotated _ smd, g) <- gs_]
              (gs_, aliases, syms) = gswust
              externs = insertFunctions mempty [(rename aliases $ qualify (smName, "Current_state_name"), ([], "char")) | ((StateMachineDeclarator smName), _) <- gs'']
              initial g = head [st ese | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n, (Just ese) <- [lab g n']]
              states g = [st ees | (_, ees) <- labNodes g]
              s_handlers e g = [(s, h) | (s, Just h@(State _, _)) <- toList (handlers e g)]
              unhandled e g = [s | (s, Just (StateAny, _)) <- toList (handlers e g)] ++ [s | (s, Nothing) <- toList (handlers e g)]
              any_handler e g = nub [h | (_, Just h@(StateAny, _)) <- toList (handlers e g)]
              events g = nub $ sort [e | (_, _, Happening {event=e@(Event _)}) <- labEdges g]
              edgeLabel (_, _, l) = l
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
