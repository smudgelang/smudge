{-# LANGUAGE NamedFieldPuns #-}

module Backends.SmudgeIR (
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
    lower,
    lowerSymTab
) where

import Grammars.Smudge (
  Annotated(..),
  StateMachine(..),
  StateMachineDeclarator(..),
  State(..),
  Event(..),
  Function(..),
  SideEffect(..),
  )
import Model (
  EnterExitState(..),
  HappeningFlag(..),
  Happening(..),
  QualifiedName,
  Qualifiable(qualify),
  TaggedName(..),
  extractWith,
  disqualifyTag,
  )
import Semantics.Operation (handlers, finalStates)
import Semantics.Solver (
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

type SmudgeIR = [Def]

data Def = FunDef QualifiedName [QualifiedName] (Binding, Ty) [DataDef] [Stmt]
         | DataDef DataDef

data DataDef = TyDef TaggedName TyDec
             | VarDef Binding (VarDec Init)

data Dec = TyDec QualifiedName TyDec
         | VarDec (VarDec UnInit)

data TyDec = EvtDec TaggedName
           | SumDec TaggedName [(QualifiedName, Maybe TaggedName)]

newtype Init a = Init a
data UnInit a = UnInit

data VarDec f = ValDec QualifiedName Ty (f Expr)
              | ListDec QualifiedName Ty (f [Expr])
              | SizeDec QualifiedName (f QualifiedName)

data Stmt = Cases Expr [(QualifiedName, [Stmt])] [Stmt]
          | If Expr [Stmt]
          | Return Expr
          | ExprS Expr

data Expr = FunCall QualifiedName [Expr]
          | Literal String
          | Null
          | Value QualifiedName
          | Assign QualifiedName Expr
          | Neq QualifiedName QualifiedName
          | SafeIndex QualifiedName QualifiedName QualifiedName String

sName :: TaggedName -> State TaggedName -> QualifiedName
sName _ (State s) = qualify s
sName smName StateAny  = qualify (smName, "ANY_STATE")

mangleEv :: Event TaggedName -> QualifiedName
mangleEv (Event evName) = extractWith seq qualify $ qualify evName
mangleEv EventEnter = qualify "enter"
mangleEv EventExit = qualify "exit"
mangleEv EventAny = qualify "any"

lower :: Bool -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], SymbolTable) -> SmudgeIR
lower debug (gs, syms) = concatMap (lowerMachine debug syms) gs

lowerSymTab :: SymbolTable -> SmudgeIR
lowerSymTab syms = [
        DataDef $ TyDef name $ EvtDec ty | (name, (b, Ty ty)) <- toList syms
    ] ++ [
        FunDef (qualify n) args f [] [] | (n, f@(_, _ :-> _)) <- toList syms
    ]
    where 
        args = map (qualify . ('a':) . show) [1..]

lowerMachine :: Bool -> SymbolTable -> (StateMachine TaggedName, Gr EnterExitState Happening) -> SmudgeIR
lowerMachine debug syms (Annotated _ (StateMachineDeclarator smName), g') = [
        DataDef $ TyDef stateEnum $ SumDec stateEnum [(qualify s, Nothing) | s <- states],
        DataDef $ VarDef Internal $ ValDec stateVar (Ty stateEnum) (Init $ Value initial),
        DataDef $ TyDef eventEnum $ SumDec eventEnum [(evt_id e, Just e) | Event e <- events]
    ] ++
        (if debug then [stateNameFun] else [])
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
        handleMessageFun
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
        handle_f = qualify (smName, "Handle_Message")
        initialize_f = qualify (smName, "initialize")
        unhandled_f e = qualify (qualify (smName, "UNHANDLED_EVENT"), mangleEv e)
        call_panic_f args = ExprS $ if debug then FunCall (qualify "panic_print") args else FunCall (qualify "panic") []
        stateEnum = TagState $ qualify (smName, "State")
        stateVar = qualify (smName, "state")
        eventEnum = TagEvent $ qualify (smName, "Event")
        evt_id e = qualify (qualify "EVID", e)
        states = [s | (_, EnterExitState {st = (State s)}) <- labNodes g]
        events = nub $ sort [e | (_, _, Happening {event=e@(Event _)}) <- labEdges g]
        s_handlers e = [(s, h) | (s, Just h@(State _, _)) <- Map.toList (handlers e g)]
        unhandled e = [s | (s, Just (StateAny, _)) <- Map.toList (handlers e g)] ++ [s | (s, Nothing) <- Map.toList (handlers e g)]
        any_handler e = nub [h | (_, Just h@(StateAny, _)) <- Map.toList (handlers e g)]
        initial = head [qualify s | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n,
                                    Just (EnterExitState {st = (State s)}) <- [lab g n']]

        stateNameFun :: Def
        stateNameFun = FunDef stateName_f [state_var] (Internal, Ty stateEnum :-> Ty char) ds es
            where ds = [VarDef Internal $ ListDec names_var (Ty char) $ Init [Literal (disqualifyTag s) | s <- states],
                        VarDef Internal $ SizeDec count_var $ Init names_var]
                  es = [Return $ SafeIndex names_var state_var count_var "INVALID_STATE"]
                  names_var = qualify "state_name"
                  count_var = qualify "state_count"
                  state_var = qualify "s"

        currentStateNameFun :: Def
        currentStateNameFun = FunDef f_name [] (syms ! TagFunction f_name) []
                                     [Return $ if debug then FunCall stateName_f [Value stateVar] else Literal ""]
            where f_name = qualify (smName, "Current_state_name")

        unhandledEventFun :: [(State TaggedName, Event TaggedName)] -> Event TaggedName -> Def
        unhandledEventFun handler e@(Event evName) = FunDef f_name eventNames (Internal, Ty evName :-> Void) ds es
            where f_name = unhandled_f e
                  name_var = qualify "event_name"
                  event_var = head eventNames
                  ds = if (not $ null handler) || not debug then [] else [VarDef Unresolved $ ValDec name_var (Ty char) (Init $ Literal $ disqualifyTag evName)]
                  es = [case handler of
                           [(s, e')] -> ExprS $ FunCall (qualify (sName smName s, mangleEv e')) (if e == e' then [Value event_var] else [])
                           [] -> call_panic_f [Literal panic_s, FunCall stateName_f [Value stateVar], Value name_var]]
                  panic_s = disqualifyTag smName ++ "[%s]: Unhandled event \"%s\"\n"

        initializeFun :: QualifiedName -> Def
        initializeFun s = FunDef initialize_f [] (Internal, Void :-> Void) ds es
            where init_var = qualify "initialized"
                  init_enum = TagState $ qualify "init_flag"
                  init_init = qualify "INITIALIZED"
                  init_uninit = qualify "UNINITIALIZED"
                  ds = [TyDef init_enum $ SumDec init_enum [(init_uninit, Nothing), (init_init, Nothing)],
                        VarDef Internal $ ValDec init_var (Ty init_enum) (Init $ Value $ init_uninit)]
                  es = [If (init_init `Neq` init_var) [
                            ExprS $ stateVar `Assign` Value s,
                            ExprS $ FunCall (qualify (s, "enter")) [],
                            ExprS $ init_var `Assign` Value init_init]]

        sendEventFun :: Event TaggedName -> Def
        sendEventFun e@(Event evName) = FunDef f_name eventNames (syms ! TagFunction f_name) [] es
            where f_name = qualify evName
                  es = [ExprS $ FunCall initialize_f [],
                        ExprS $ FunCall handle_f [Value $ evt_id evName, Value event_var]]
                  event_var = head eventNames

        handleEventFun :: Event TaggedName -> [(State TaggedName, (State TaggedName, Event TaggedName))] -> [State TaggedName] -> Def
        handleEventFun e@(Event evName) ss unss = FunDef f_name eventNames (Internal, snd $ syms ! TagFunction (qualify evName)) [] es
            where f_name = qualify (qualify evName, "handle")
                  es = [Cases (Value stateVar) cases defaults]
                  event_var = head eventNames
                  call_unhandled = ExprS $ FunCall (unhandled_f e) [Value event_var]
                  call_ev_in s e' = ExprS $ FunCall (qualify (s, mangleEv e')) (if e == e' then [Value event_var] else [])
                  cases = [(qualify s, [call_ev_in s' e']) | (State s, (State s', e')) <- ss]
                       ++ [(qualify s, [call_unhandled]) | (State s) <- unss]
                  defaults = [call_unhandled]

        handleMessageFun :: Def
        handleMessageFun = FunDef handle_f [evId, evt] (Exported, Ty eventEnum :-> Ty (TagBuiltin $ qualify "void") :-> Void) [] es
            where es = [Cases (Value evId) cases defaults,
                        ExprS $ FunCall (qualify "free") [Value evt]]
                  evId = qualify "eventId"
                  evt = qualify "event"
                  cases = [(evt_id e, [ExprS $ FunCall (qualify (qualify e, "handle")) [Value evt]]) | Event e <- events]
                  defaults = [call_panic_f [Literal panic_s, FunCall stateName_f [Value stateVar], Literal ""]]
                  panic_s = disqualifyTag smName ++ "[%s]: Invalid event ID\n"

        stateEventFun :: State TaggedName -> Happening -> State TaggedName -> Bool -> Def
        stateEventFun st h st' do_exit = FunDef f_name eventNames (Internal, ty) [] $ map ExprS es
            where f_name = qualify (sName smName st, mangleEv $ event h)
                  ty = case event h of
                              (Event t) -> Ty t :-> Void
                              otherwise -> Void :-> Void

                  event_var = head eventNames
                  dest_state = qualify $ sName smName st'
                  call_exit = FunCall (qualify (sName smName st, "exit")) []
                  assign_state = stateVar `Assign` Value dest_state
                  call_enter = FunCall (qualify (sName smName st', "enter")) []

                  isEventTy :: Ty -> Event TaggedName -> Bool
                  isEventTy a (Event e) = a == snd (syms ! e)
                  isEventTy _ _ = False

                  psOf (Void :-> _) = []
                  psOf (p    :-> _) = [if isEventTy p (event h) then Value event_var else Null]
                  apply_se (f, FuncTyped _) = undefined -- See ticket #15, harder than it seems at first.
                  apply_se (f, _) = FunCall (qualify f) (psOf (snd $ syms ! f))
                  es = case h of
                         (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ (if do_exit then [call_exit] else []) ++ [assign_state, call_enter]
                         (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

        anyExitFun :: [State TaggedName] -> Def
        anyExitFun ss = FunDef f_name [] (Internal, Void :-> Void) [] es
            where f_name = qualify (sName smName StateAny, mangleEv EventExit)
                  es = [Cases (Value stateVar) cases []]
                  cases = [(qualify s, [ExprS $ FunCall (qualify (s, mangleEv EventExit)) []]) | (State s) <- ss]
