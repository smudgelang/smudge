-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

{-# LANGUAGE NamedFieldPuns #-}

module Language.Smudge.Backends.SmudgeIR (
    SmudgeIR,
    Def(..),
    DataDef(..),
    Dec(..),
    TyDec(..),
    Init(..),
    UnInit,
    VarDec(..),
    Stmt(..),
    Expr(..),
    Var(..),
    lower,
    lowerSymTab
) where

import Language.Smudge.Backends.Backend (Config(..))
import Language.Smudge.Grammar (
  StateMachine(..),
  State(..),
  Event(..),
  Function(..),
  SideEffect(..),
  )
import Language.Smudge.Semantics.Model (
  EnterExitState(..),
  HappeningFlag(..),
  Happening(..),
  QualifiedName,
  Qualifiable(qualify),
  TaggedName,
  Tagged(..),
  extractWith,
  disqualifyTag,
  )
import Language.Smudge.Semantics.Operation (handlers, finalStates)
import Language.Smudge.Semantics.Solver (
  Ty(..), 
  Binding(..), 
  toList,
  SymbolTable, 
  (!),
  )

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (labNodes, labEdges, lab, out, suc, insEdges, nodes, delNodes)
import Data.List (nub, sort)
import qualified Data.Map as Map (toList)

type SmudgeIR x = [Def x]

data Def x = FunDef x [x] (Binding, Ty) [DataDef x] [Stmt x]
           | DataDef (DataDef x)

data DataDef x = TyDef (Tagged x) (TyDec x)
               | VarDef Binding (VarDec Init x)

data Dec x = TyDec x (TyDec x)
           | VarDec (VarDec UnInit x)

data TyDec x = EvtDec (Tagged x)
             | SumDec (Tagged x) [(x, Maybe (Tagged x))]

newtype Init a = Init a
data UnInit a = UnInit

data VarDec f x = ValDec x Ty (f (Expr x))
                | SumVDec x Ty (f (x, Expr x))
                | ListDec x Ty (f [Expr x])
                | SizeDec x (f x)

data Stmt x = Cases (Expr x) [(x, [Stmt x])] [Stmt x]
            | If (Expr x) [Stmt x]
            | Return (Expr x)
            | ExprS (Expr x)

data Expr x = FunCall x [Expr x]
            | Literal String
            | Null
            | Value (Var x)
            | Assign (Var x) (Expr x)
            | Neq x x
            | SafeIndex (Var x) (Var x) (Var x) String

data Var x = Var x
           | SumVar x
           | Field (Var x) x

sName :: TaggedName -> State TaggedName -> QualifiedName
sName _ (State s) = qualify s
sName smName StateAny  = qualify (smName, "ANY_STATE")

mangleEv :: Event TaggedName -> QualifiedName
mangleEv (Event evName) = extractWith seq qualify $ qualify evName
mangleEv EventEnter = qualify "enter"
mangleEv EventExit = qualify "exit"
mangleEv EventAny = qualify "any"

events_for :: Gr EnterExitState Happening -> [Event TaggedName]
events_for g = nub $ sort [e | (_, _, Happening {event=e@(Event _)}) <- labEdges g]

lower :: Config -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], SymbolTable) -> SmudgeIR QualifiedName
lower cfg (gs, syms) = concatMap (lowerMachine cfg syms) gs

lowerSymTab :: [(StateMachine TaggedName, Gr EnterExitState Happening)] -> SymbolTable -> SmudgeIR QualifiedName
lowerSymTab gs syms = [
        DataDef $ TyDef name $ EvtDec ty | (name, (b, Ty ty)) <- toList syms
    ] ++ [
        DataDef $ TyDef eventEnum $ SumDec eventEnum [(qualify (qualify "EVID", e), Just e) | Event e <- events_for g] -- a kludge to get it into the header
            | (StateMachine smName, g) <- gs, let eventEnum = (\(Ty p :-> r) -> p) $ snd (syms ! TagFunction (qualify (smName, "Handle_Message")))
    ] ++ [
        FunDef (qualify n) args f [] [] | (n, f@(_, _ :-> _)) <- toList syms
    ]
    where 
        args = map (qualify . ('a':) . show) [1..]

lowerMachine :: Config -> SymbolTable -> (StateMachine TaggedName, Gr EnterExitState Happening) -> SmudgeIR QualifiedName
lowerMachine cfg syms (StateMachine smName, g') = [
        DataDef $ TyDef stateEnum $ SumDec stateEnum [(st_id s, Nothing) | s <- states],
        DataDef $ VarDef Internal $ ValDec stateVar (Ty stateEnum) (Init $ Value $ Var $ st_id initial)
    ] ++
        (if debug cfg then [stateNameFun, eventNameFun] else [])
      ++ [
        currentStateNameFun
    ] ++ [
        anyExitFun [st | (n, EnterExitState {st, ex = (_:_)}) <- labNodes g, n `notElem` finalStates g] | (_, EnterExitState {st = StateAny}) <- labNodes g
    ] ++ [
        unhandledEventFun (any_handler e) e | e <- events
    ] ++ [
        initializeFun initial
    ] ++ concat [
        [sendEventFun e, handleEventFun e (s_handlers e) (unhandled e)] | e <- events
    ] ++ [
        handleMessageFun,
        freeMessageFun
    ] ++ [
        stateEventFun st h s' (st == StateAny || not (null ex)) | (n, EnterExitState {st, ex}) <- labNodes g,
         (_, n', h) <- out g n,
         Just EnterExitState {st = s'} <- [lab g n'],
         case st of State _ -> True; StateAny -> True; _ -> False,
         case s' of State _ -> True; StateAny -> True; _ -> False
    ]
    where
        g = insEdges ([(n, n, Happening EventExit ex [NoTransition])
                       | (n, EnterExitState {st = State _, ex = ex@(_:_)}) <- labNodes $ delNodes (finalStates g' ++ [n | n <- nodes g', (_, _, Happening EventExit _ _) <- out g' n]) g'] ++
                      [(n, n, Happening EventEnter en [NoTransition])
                       | (n, EnterExitState {en, st = State _}) <- labNodes $ delNodes [n | n <- nodes g', (_, _, Happening EventEnter _ _) <- out g' n] g']) g'
        eventNames = map qualify $ ["e"] ++ map (('e':) . show) [2..]
        char = TagBuiltin $ qualify "char"
        stateName_f = qualify (smName, "State_name")
        eventName_f = qualify (smName, "Event_name")
        send_f = qualify (smName, "Send_Message")
        handle_f = qualify (smName, "Handle_Message")
        initialize_f = qualify (smName, "initialize")
        unhandled_f e = qualify (qualify (smName, "UNHANDLED_EVENT"), mangleEv e)
        call_panic_f args = ExprS $ if debug cfg then FunCall (qualify "panic_print") args else FunCall (qualify "panic") []
        call_print_f args = ExprS $ if debug cfg then FunCall (qualify "debug_print") args else FunCall (qualify "debug_print") [Literal "" | arg <- args]
        wrap_name = qualify "wrapper"
        stateEnum = TagState $ qualify (smName, "State")
        stateVar = qualify (smName, "state")
        eventEnum = (\(Ty p :-> r) -> p) $ snd (syms ! TagFunction handle_f)
        evt_id e = qualify (qualify "EVID", e)
        st_id s = qualify (qualify "STID", s)
        states = [s | (_, EnterExitState {st = (State s)}) <- labNodes g]
        events = events_for g
        s_handlers e = [(s, h) | (s, Just h@(State _, _)) <- Map.toList (handlers e g)]
        unhandled e = [s | (s, Just (StateAny, _)) <- Map.toList (handlers e g)] ++ [s | (s, Nothing) <- Map.toList (handlers e g)]
        any_handler e = nub [h | (_, Just h@(StateAny, _)) <- Map.toList (handlers e g)]
        initial = head [qualify s | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n,
                                    Just (EnterExitState {st = (State s)}) <- [lab g n']]

        stateNameFun :: Def QualifiedName
        stateNameFun = FunDef stateName_f [state_var] (Internal, Ty stateEnum :-> Ty char) ds es
            where ds = [VarDef Internal $ ListDec names_var (Ty char) $ Init [Literal (disqualifyTag s) | s <- states],
                        VarDef Internal $ SizeDec count_var $ Init names_var]
                  es = [Return $ SafeIndex (Var names_var) (Var state_var) (Var count_var) "INVALID_STATE"]
                  names_var = qualify "state_name"
                  count_var = qualify "state_count"
                  state_var = qualify "s"

        eventNameFun :: Def QualifiedName
        eventNameFun = FunDef eventName_f [event_var] (Internal, Ty eventEnum :-> Ty char) ds es
            where ds = [VarDef Internal $ ListDec names_var (Ty char) $ Init [Literal (disqualifyTag e) | Event e <- events],
                        VarDef Internal $ SizeDec count_var $ Init names_var]
                  es = [Return $ SafeIndex (Var names_var) (SumVar event_var) (Var count_var) "INVALID_EVENT"]
                  names_var = qualify "event_name"
                  count_var = qualify "event_count"
                  event_var = qualify "e"

        currentStateNameFun :: Def QualifiedName
        currentStateNameFun = FunDef f_name [] (syms ! TagFunction f_name) []
                                     [Return $ if debug cfg then FunCall stateName_f [Value $ Var stateVar] else Literal ""]
            where f_name = qualify (smName, "Current_state_name")

        unhandledEventFun :: [(State TaggedName, Event TaggedName)] -> Event TaggedName -> Def QualifiedName
        unhandledEventFun handler e@(Event evName) = FunDef f_name eventNames (Internal, Ty evName :-> Void) ds es
            where f_name = unhandled_f e
                  name_var = qualify "event_name"
                  event_var = head eventNames
                  ds = if (not $ null handler) || not (debug cfg) then [] else [VarDef Unresolved $ SumVDec wrap_name (Ty eventEnum) $ Init (evt_id evName, Value $ Var event_var)]
                  es = [case handler of
                           [(s, e')] -> ExprS $ FunCall (qualify (sName smName s, mangleEv e')) (if e == e' then [Value $ Var event_var] else [])
                           [] -> call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], FunCall eventName_f [Value $ Var wrap_name]]]
                  panic_s = disqualifyTag smName ++ "[%s]: Unhandled event \"%s\"\n"

        initializeFun :: QualifiedName -> Def QualifiedName
        initializeFun s = FunDef initialize_f [] (Internal, Void :-> Void) ds es
            where init_var = qualify "initialized"
                  init_enum = TagState $ qualify "init_flag"
                  init_init = qualify "INITIALIZED"
                  init_uninit = qualify "UNINITIALIZED"
                  ds = [TyDef init_enum $ SumDec init_enum [(init_uninit, Nothing), (init_init, Nothing)],
                        VarDef Internal $ ValDec init_var (Ty init_enum) (Init $ Value $ Var init_uninit)]
                  es = [If (init_init `Neq` init_var) [
                            ExprS $ Var stateVar `Assign` Value (Var $ st_id s),
                            ExprS $ FunCall (qualify (s, "enter")) [],
                            ExprS $ Var init_var `Assign` Value (Var init_init)]]

        sendEventFun :: Event TaggedName -> Def QualifiedName
        sendEventFun e@(Event evName) = FunDef f_name eventNames (syms ! TagFunction f_name) ds es
            where f_name = qualify evName
                  ds = [VarDef Unresolved $ SumVDec wrap_name (Ty eventEnum) $ Init (evt_id evName, Value $ Var event_var)]
                  es = [ExprS $ FunCall send_f [Value $ Var wrap_name]]
                  event_var = head eventNames

        handleEventFun :: Event TaggedName -> [(State TaggedName, (State TaggedName, Event TaggedName))] -> [State TaggedName] -> Def QualifiedName
        handleEventFun e@(Event evName) ss unss = FunDef f_name eventNames (Internal, snd $ syms ! TagFunction (qualify evName)) [] es
            where f_name = qualify (qualify evName, "handle")
                  es = [Cases (Value $ Var stateVar) cases defaults]
                  event_var = head eventNames
                  call_unhandled = ExprS $ FunCall (unhandled_f e) [Value $ Var event_var]
                  call_ev_in s e' = ExprS $ FunCall (qualify (s, mangleEv e')) (if e == e' then [Value $ Var event_var] else [])
                  cases = [(st_id s, [call_ev_in s' e']) | (State s, (State s', e')) <- ss]
                       ++ [(st_id s, [call_unhandled]) | (State s) <- unss]
                  defaults = [call_unhandled]

        handleMessageFun :: Def QualifiedName
        handleMessageFun = FunDef handle_f [wrap_name] (syms ! TagFunction handle_f) [] es
            where es = [ExprS $ FunCall initialize_f []] ++
                       (if logEvent cfg then [call_print_f [Literal format_s, FunCall stateName_f [Value $ Var stateVar], FunCall eventName_f [Value $ Var wrap_name]]] else []) ++
                       [Cases (Value $ SumVar wrap_name) cases defaults]
                  format_s = disqualifyTag smName ++ "[%s]: Handling \"%s\"\n"
                  cases = [(evt_id e, [ExprS $ FunCall (qualify (qe, "handle")) [Value $ Field (SumVar wrap_name) qe]]) | Event e <- events, let qe = qualify e]
                  defaults = [call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], Literal ""]]
                  panic_s = disqualifyTag smName ++ "[%s]: Invalid event ID\n"

        freeMessageFun :: Def QualifiedName
        freeMessageFun = FunDef f_name [wrap_name] (syms ! TagFunction f_name) [] es
            where f_name = qualify (smName, "Free_Message")
                  es = [Cases (Value $ SumVar wrap_name) cases defaults]
                  cases = [(evt_id e, [ExprS $ FunCall (qualify "free") [Value $ Field (SumVar wrap_name) $ qualify e]]) | Event e <- events]
                  defaults = [call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], Literal ""]]
                  panic_s = disqualifyTag smName ++ "[%s]: Invalid event ID\n"

        stateEventFun :: State TaggedName -> Happening -> State TaggedName -> Bool -> Def QualifiedName
        stateEventFun st h st' do_exit = FunDef f_name eventNames (Internal, ty) [] $ map ExprS es
            where f_name = qualify (sName smName st, mangleEv $ event h)
                  ty = case event h of
                              (Event t) -> Ty t :-> Void
                              otherwise -> Void :-> Void

                  event_var = head eventNames
                  dest_state = qualify $ sName smName st'
                  call_exit = FunCall (qualify (sName smName st, "exit")) []
                  assign_state = Var stateVar `Assign` Value (Var $ st_id dest_state)
                  call_enter = FunCall (qualify (sName smName st', "enter")) []

                  isEventTy :: Ty -> Event TaggedName -> Bool
                  isEventTy a (Event e) = a == snd (syms ! e)
                  isEventTy _ _ = False

                  psOf (Void :-> _) = []
                  psOf (p    :-> _) = [if isEventTy p (event h) then Value $ Var event_var else Null]
                  apply_se (f, FuncTyped _) = undefined -- See ticket #15, harder than it seems at first.
                  apply_se (f, _) = FunCall (qualify f) (psOf (snd $ syms ! f))
                  es = case h of
                         (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ (if do_exit then [call_exit] else []) ++ [assign_state, call_enter]
                         (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

        anyExitFun :: [State TaggedName] -> Def QualifiedName
        anyExitFun ss = FunDef f_name [] (Internal, Void :-> Void) [] es
            where f_name = qualify (sName smName StateAny, mangleEv EventExit)
                  es = [Cases (Value $ Var stateVar) cases []]
                  cases = [(st_id s, [ExprS $ FunCall (qualify (s, mangleEv EventExit)) []]) | (State s) <- ss]
