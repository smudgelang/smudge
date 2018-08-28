-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

{-# LANGUAGE NamedFieldPuns #-}

module Language.Smudge.Backends.SmudgeIR (
    SmudgeIR,
    Def(..),
    Ty(..),
    DataDef(..),
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
  events_for,
  states_for,
  )
import Language.Smudge.Passes.Passes (afold)
import Language.Smudge.Semantics.Operation (handlers, finalStates)
import Language.Smudge.Semantics.Solver (
  Binding(..),
  SymbolTable,
  )
import qualified Language.Smudge.Semantics.Solver as Solver (Ty(..))

import Control.Arrow ((***), second)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (labNodes, lab, out, suc, insEdges, nodes, delNodes)
import Data.List (nub)
import Data.Map (Map, empty, insert, (!), toList)
import Data.Set (member)

seqtup :: Applicative f => (f a, f b) -> f (a, b)
seqtup (a, b) = (,) <$> a <*> b

type SmudgeIR x = [Def x]

data Def x = FunDef x [x] (Binding, Ty x) [DataDef x] [Stmt x]
           | DataDef (DataDef x)

instance Functor Def where
    fmap f (FunDef name ps (b, ty) ds ss) = FunDef (f name) (map f ps) (b, fmap f ty) (map (fmap f) ds) (map (fmap f) ss)
    fmap f (DataDef d) = DataDef $ fmap f d

instance Foldable Def where
    foldMap f (FunDef name ps (b, ty) ds ss) = f name `mappend` foldMap f ps `mappend` foldMap f ty `mappend` foldMap (foldMap f) ds `mappend` foldMap (foldMap f) ss
    foldMap f (DataDef d) = foldMap f d

instance Traversable Def where
    traverse f (FunDef name ps (b, ty) ds ss) = FunDef <$> f name <*> traverse f ps <*> ((,) b <$> traverse f ty) <*> traverse (traverse f) ds <*> traverse (traverse f) ss
    traverse f (DataDef d) = DataDef <$> traverse f d

data Ty x = Void
        | Ty (Tagged x)
        | Ty x :-> Ty x
    deriving (Eq, Ord)

infixr 7 :->

instance Functor Ty where
    fmap f Void = Void
    fmap f (Ty n) = Ty $ fmap f n
    fmap f (tau :-> tau') = fmap f tau :-> fmap f tau'

instance Foldable Ty where
    foldMap f Void = mempty
    foldMap f (Ty n) = foldMap f n
    foldMap f (tau :-> tau') = foldMap f tau `mappend` foldMap f tau'

instance Traversable Ty where
    traverse f Void = pure Void
    traverse f (Ty n) = Ty <$> traverse f n
    traverse f (tau :-> tau') = (:->) <$> traverse f tau <*> traverse f tau'

data DataDef x = TyDef (Tagged x) (TyDec x)
               | VarDef Binding (VarDec Init x)

instance Functor DataDef where
    fmap f (TyDef x d) = TyDef (fmap f x) (fmap f d)
    fmap f (VarDef b d) = VarDef b (fmap f d)

instance Foldable DataDef where
    foldMap f (TyDef x d) = foldMap f x `mappend` foldMap f d
    foldMap f (VarDef b d) = foldMap f d

instance Traversable DataDef where
    traverse f (TyDef x d) = TyDef <$> traverse f x <*> traverse f d
    traverse f (VarDef b d) = VarDef b <$> traverse f d

data TyDec x = EvtDec (Tagged x)
             | SumDec (Tagged x) [(x, Maybe (Tagged x))]

instance Functor TyDec where
    fmap f (EvtDec ty) = EvtDec $ fmap f ty
    fmap f (SumDec x cs) = SumDec (fmap f x) (map (f *** fmap (fmap f)) cs)

instance Foldable TyDec where
    foldMap f (EvtDec ty) = foldMap f ty
    foldMap f (SumDec x cs) = foldMap f x `mappend` foldMap (uncurry mappend . (f *** foldMap (foldMap f))) cs

instance Traversable TyDec where
    traverse f (EvtDec ty) = EvtDec <$> traverse f ty
    traverse f (SumDec x cs) = SumDec <$> traverse f x <*> traverse (seqtup . (f *** traverse (traverse f))) cs

newtype Init a = Init a

instance Functor Init where
    fmap f (Init a) = Init (f a)

instance Foldable Init where
    foldMap f (Init a) = f a

instance Traversable Init where
    traverse f (Init a) = Init <$> f a

data UnInit a = UnInit

instance Functor UnInit where
    fmap f UnInit = UnInit

instance Foldable UnInit where
    foldMap f UnInit = mempty

instance Traversable UnInit where
    traverse f UnInit = pure UnInit

data VarDec f x = ValDec x (Ty x) (f (Expr x))
                | SumVDec x (Ty x) (f (x, Expr x))
                | ListDec x (Ty x) (f [Expr x])
                | SizeDec x (f x)

instance Functor f => Functor (VarDec f) where
    fmap f (ValDec x ty i) = ValDec (f x) (fmap f ty) (fmap (fmap f) i)
    fmap f (SumVDec x ty i) = SumVDec (f x) (fmap f ty) (fmap (f *** fmap f) i)
    fmap f (ListDec x ty i) = ListDec (f x) (fmap f ty) (fmap (map (fmap f)) i)
    fmap f (SizeDec x i) = SizeDec (f x) (fmap f i)

instance Foldable f => Foldable (VarDec f) where
    foldMap f (ValDec x ty i) = f x `mappend` foldMap f ty `mappend` foldMap (foldMap f) i
    foldMap f (SumVDec x ty i) = f x `mappend` foldMap f ty `mappend` foldMap (uncurry mappend . (f *** foldMap f)) i
    foldMap f (ListDec x ty i) = f x `mappend` foldMap f ty `mappend` foldMap (foldMap (foldMap f)) i
    foldMap f (SizeDec x i) = f x `mappend` foldMap f i

instance Traversable f => Traversable (VarDec f) where
    traverse f (ValDec x ty i) = ValDec <$> f x <*> traverse f ty <*> traverse (traverse f) i
    traverse f (SumVDec x ty i) = SumVDec <$> f x <*> traverse f ty <*> traverse (seqtup . (f *** traverse f)) i
    traverse f (ListDec x ty i) = ListDec <$> f x <*> traverse f ty <*> traverse (traverse (traverse f)) i
    traverse f (SizeDec x i) = SizeDec <$> f x <*> traverse f i

data Stmt x = Cases (Expr x) [(x, [Stmt x])] [Stmt x]
            | If (Expr x) [Stmt x]
            | Return (Expr x)
            | ExprS (Expr x)

instance Functor Stmt where
    fmap f (Cases e cs ds) = Cases (fmap f e) (map (f *** map (fmap f)) cs) (map (fmap f) ds)
    fmap f (If e ss) = If (fmap f e) (map (fmap f) ss)
    fmap f (Return e) = Return $ fmap f e
    fmap f (ExprS e) = ExprS $ fmap f e

instance Foldable Stmt where
    foldMap f (Cases e cs ds) = foldMap f e `mappend` foldMap (uncurry mappend . (f *** foldMap (foldMap f))) cs `mappend` foldMap (foldMap f) ds
    foldMap f (If e ss) = foldMap f e `mappend` foldMap (foldMap f) ss
    foldMap f (Return e) = foldMap f e
    foldMap f (ExprS e) = foldMap f e

instance Traversable Stmt where
    traverse f (Cases e cs ds) = Cases <$> traverse f e <*> traverse (seqtup . (f *** traverse (traverse f))) cs <*> traverse (traverse f) ds
    traverse f (If e ss) = If <$> traverse f e <*> traverse (traverse f) ss
    traverse f (Return e) = Return <$> traverse f e
    traverse f (ExprS e) = ExprS <$> traverse f e

data Expr x = FunCall x [Expr x]
            | Literal String
            | Null
            | Value (Var x)
            | UnusedValue (Var x)
            | Assign (Var x) (Expr x)
            | Neq x x
            | SafeIndex (Var x) (Var x) (Var x) String

instance Functor Expr where
    fmap f (FunCall x es) = FunCall (f x) (map (fmap f) es)
    fmap f (Literal v) = Literal v
    fmap f  Null = Null
    fmap f (Value v) = Value $ fmap f v
    fmap f (UnusedValue v) = UnusedValue $ fmap f v
    fmap f (Assign v e) = Assign (fmap f v) (fmap f e)
    fmap f (Neq x1 x2) = Neq (f x1) (f x2)
    fmap f (SafeIndex a i b d) = SafeIndex (fmap f a) (fmap f i) (fmap f b) d

instance Foldable Expr where
    foldMap f (FunCall x es) = f x `mappend` foldMap (foldMap f) es
    foldMap f (Literal v) = mempty
    foldMap f  Null = mempty
    foldMap f (Value v) = foldMap f v
    foldMap f (UnusedValue v) = foldMap f v
    foldMap f (Assign v e) = foldMap f v `mappend` foldMap f e
    foldMap f (Neq x1 x2) = f x1 `mappend` f x2
    foldMap f (SafeIndex a i b d) = foldMap f a `mappend` foldMap f i `mappend` foldMap f b

instance Traversable Expr where
    traverse f (FunCall x es) = FunCall <$> f x <*> traverse (traverse f) es
    traverse f (Literal v) = pure $ Literal v
    traverse f  Null = pure Null
    traverse f (Value v) = Value <$> traverse f v
    traverse f (UnusedValue v) = UnusedValue <$> traverse f v
    traverse f (Assign v e) = Assign <$> traverse f v <*> traverse f e
    traverse f (Neq x1 x2) = Neq <$> f x1 <*> f x2
    traverse f (SafeIndex a i b d) = SafeIndex <$> traverse f a <*> traverse f i <*> traverse f b <*> pure d

data Var x = Var x
           | SumVar x
           | Field (Var x) x

instance Functor Var where
    fmap f (Var x) = Var $ f x
    fmap f (SumVar x) = SumVar $ f x
    fmap f (Field v x) = Field (fmap f v) (f x)

instance Foldable Var where
    foldMap f (Var x) = f x
    foldMap f (SumVar x) = f x
    foldMap f (Field v x) = foldMap f v `mappend` f x

instance Traversable Var where
    traverse f (Var x) = Var <$> f x
    traverse f (SumVar x) = SumVar <$> f x
    traverse f (Field v x) = Field <$> traverse f v <*> f x

sName :: TaggedName -> State TaggedName -> QualifiedName
sName _ (State s) = qualify s
sName smName StateAny  = qualify (smName, "ANY_STATE")

mangleEv :: Event TaggedName -> QualifiedName
mangleEv (Event evName) = extractWith seq qualify $ qualify evName
mangleEv EventEnter = qualify "enter"
mangleEv EventExit = qualify "exit"
mangleEv EventAny = qualify "any"

lower :: Config -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], SymbolTable) -> SmudgeIR QualifiedName
lower cfg (gs, syms) = concatMap (lowerMachine cfg syms) gs

lowerTy :: Solver.Ty -> Ty QualifiedName
lowerTy      Solver.Void  = Void
lowerTy     (Solver.Ty x) = Ty x
lowerTy (t Solver.:-> t') = lowerTy t :-> lowerTy t'
lowerTy                 t = error $ "Tried to lower '" ++ show t ++ "'.  This is a bug in smudge.\n"

lowerSolverSyms :: SymbolTable -> Map TaggedName (Binding, Ty QualifiedName)
lowerSolverSyms = afold (uncurry insert . second (second lowerTy)) empty

lowerSymTab :: [(StateMachine TaggedName, Gr EnterExitState Happening)] -> SymbolTable -> SmudgeIR QualifiedName
lowerSymTab gs ssyms = [
        DataDef $ TyDef name $ EvtDec ty | (name, (b, Ty ty)) <- symslist
    ] ++ [
        DataDef $ TyDef eventEnum $ SumDec eventEnum [(qualify (qualify "EVID", e), Just e) | Event e <- events_for g] -- a kludge to get it into the header
            | (StateMachine smName, g) <- gs, let eventEnum = (\(Ty p :-> r) -> p) $ snd (syms ! TagFunction (qualify (smName, "Handle_Message")))
    ] ++ [
        FunDef (qualify n) args f [] [] | (n, f@(_, _ :-> _)) <- symslist
    ]
    where
        args = map (qualify . ('a':) . show) [1..127]
        syms = lowerSolverSyms ssyms
        symslist = toList syms

lowerMachine :: Config -> SymbolTable -> (StateMachine TaggedName, Gr EnterExitState Happening) -> SmudgeIR QualifiedName
lowerMachine cfg ssyms (StateMachine smName, g') = [
        DataDef $ TyDef stateEnum $ SumDec stateEnum [(st_id s, Nothing) | State s <- states],
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
        syms = lowerSolverSyms ssyms
        g = insEdges ([(n, n, Happening EventExit ex [NoTransition])
                       | (n, EnterExitState {st = State _, ex = ex@(_:_)}) <- labNodes $ delNodes (finalStates g' ++ [n | n <- nodes g', (_, _, Happening EventExit _ _) <- out g' n]) g'] ++
                      [(n, n, Happening EventEnter en [NoTransition])
                       | (n, EnterExitState {en, st = State _}) <- labNodes $ delNodes [n | n <- nodes g', (_, _, Happening EventEnter _ _) <- out g' n] g']) g'
        eventNames = map qualify $ ["e"] ++ map (('e':) . show) [2..127]
        char = TagBuiltin $ qualify "char"
        stateName_f = qualify (smName, "State_name")
        eventName_f = qualify (smName, "Event_name")
        send_f = qualify (smName, "Send_Message")
        handle_f = qualify (smName, "Handle_Message")
        initialize_f = qualify (smName, "initialize")
        unhandled_f e = qualify (qualify (smName, "UNHANDLED_EVENT"), mangleEv e)
        call_panic_f args = if debug cfg then [ExprS $ FunCall (qualify "panic_print") args] else [ExprS $ FunCall (qualify "panic") []]
        call_print_f args = if debug cfg then [ExprS $ FunCall (qualify "debug_print") args] else []
        wrap_name = qualify "wrapper"
        stateEnum = TagState $ qualify (smName, "State")
        stateVar = qualify (smName, "state")
        eventEnum = (\(Ty p :-> r) -> p) $ snd (syms ! TagFunction handle_f)
        evt_id e = qualify (qualify "EVID", e)
        st_id s = qualify (qualify "STID", s)
        states = states_for g
        events = events_for g
        s_handlers e = [(s, h) | (s, Just h@(State _, _)) <- toList (handlers e g)]
        unhandled e = [s | (s, Just (StateAny, _)) <- toList (handlers e g)] ++ [s | (s, Nothing) <- toList (handlers e g)]
        any_handler e = nub [h | (_, Just h@(StateAny, _)) <- toList (handlers e g)]
        initial = head [qualify s | (n, EnterExitState {st = StateEntry}) <- labNodes g, n' <- suc g n,
                                    Just (EnterExitState {st = (State s)}) <- [lab g n']]

        stateNameFun :: Def QualifiedName
        stateNameFun = FunDef stateName_f [state_var] (Internal, Ty stateEnum :-> Ty char) ds es
            where ds = [VarDef Internal $ ListDec names_var (Ty char) $ Init [Literal (disqualifyTag s) | State s <- states],
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
                  es =  (if (null ds) then [ExprS (UnusedValue $ Var event_var)] else []) ++ (case handler of
                           [(s, e')] -> [ExprS $ FunCall (qualify (sName smName s, mangleEv e')) (if e == e' then [Value $ Var event_var] else [])]
                           [] -> call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], FunCall eventName_f [Value $ Var wrap_name]])
                  panic_s = disqualifyTag smName ++ "{%s[%s]}: Unhandled event\n"

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
                  es = [ExprS (UnusedValue $ Var event_var), Cases (Value $ Var stateVar) cases defaults]
                  event_var = head eventNames
                  call_unhandled = ExprS $ FunCall (unhandled_f e) [Value $ Var event_var]
                  call_ev_in s e' = ExprS $ FunCall (qualify (s, mangleEv e')) (if e == e' then [Value $ Var event_var] else [])
                  cases = [(st_id s, [call_ev_in s' e']) | (State s, (State s', e')) <- ss]
                       ++ [(st_id s, [call_unhandled]) | (State s) <- unss]
                  defaults = [call_unhandled]

        handleMessageFun :: Def QualifiedName
        handleMessageFun = FunDef handle_f [wrap_name] (syms ! TagFunction handle_f) [] es
            where es = [ExprS $ FunCall initialize_f []] ++
                       (if all (`member` logEvent cfg) events then logAnEvent
                        else if null logCases then []
                        else [Cases (Value $ SumVar wrap_name) logCases []]) ++
                       [Cases (Value $ SumVar wrap_name) cases defaults]
                  logCases = [(evt_id e, logAnEvent) | Event e <- events, debug cfg, Event e `member` logEvent cfg]
                  logAnEvent = call_print_f [Literal format_s, FunCall stateName_f [Value $ Var stateVar], FunCall eventName_f [Value $ Var wrap_name]]
                  format_s = disqualifyTag smName ++ "{%s[%s]}: Handling\n"
                  cases = [(evt_id e, [ExprS $ FunCall (qualify (qe, "handle")) [Value $ Field (SumVar wrap_name) qe]]) | Event e <- events, let qe = qualify e]
                  defaults = call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], Literal ""]
                  panic_s = disqualifyTag smName ++ "{%s}: Invalid event ID\n"

        freeMessageFun :: Def QualifiedName
        freeMessageFun = FunDef f_name [wrap_name] (syms ! TagFunction f_name) [] es
            where f_name = qualify (smName, "Free_Message")
                  es = [Cases (Value $ SumVar wrap_name) cases defaults]
                  cases = [(evt_id e, [ExprS $ FunCall (qualify "free") [Value $ Field (SumVar wrap_name) $ qualify e]]) | Event e <- events]
                  defaults = call_panic_f [Literal panic_s, FunCall stateName_f [Value $ Var stateVar], Literal ""]
                  panic_s = disqualifyTag smName ++ "{%s}: Invalid event ID\n"

        stateEventFun :: State TaggedName -> Happening -> State TaggedName -> Bool -> Def QualifiedName
        stateEventFun st h st' do_exit = FunDef f_name eventNames (Internal, ty) [] $ logAState ++ map ExprS vs ++ map ExprS es
            where f_name = qualify (sName smName st, mangleEv $ event h)
                  ty = case event h of
                              (Event t) -> Ty t :-> Void
                              otherwise -> Void :-> Void

                  logAState = if not $ event h == EventEnter && st `member` logState cfg then []
                              else call_print_f [Literal format_s, FunCall stateName_f [Value $ Var stateVar], Literal ""]
                  format_s = disqualifyTag smName ++ "{%s}: Entering state\n"

                  event_var = head eventNames
                  dest_state = qualify $ sName smName st'
                  call_exit = FunCall (qualify (sName smName st, "exit")) []
                  assign_state = Var stateVar `Assign` Value (Var $ st_id dest_state)
                  call_enter = FunCall (qualify (sName smName st', "enter")) []

                  isEventTy :: Ty QualifiedName -> Event TaggedName -> Bool
                  isEventTy a (Event e) = a == snd (syms ! e)
                  isEventTy _ _ = False

                  psOf (Void :-> _) = []
                  psOf (p    :-> _) = [if isEventTy p (event h) then Value $ Var event_var else Null]
                  apply_se (f, FuncTyped _) = undefined -- See ticket #15, harder than it seems at first.
                  apply_se (f, _) = FunCall (qualify f) (psOf (snd $ syms ! f))
                  vs = case event h of
                              (Event t) -> [UnusedValue $ Var event_var]
                              otherwise -> []
                  es = case h of
                         (Happening _ ses [])                        -> [apply_se se | se <- ses] ++ (if do_exit then [call_exit] else []) ++ [assign_state, call_enter]
                         (Happening _ ses fs) | elem NoTransition fs -> [apply_se se | se <- ses]

        anyExitFun :: [State TaggedName] -> Def QualifiedName
        anyExitFun ss = FunDef f_name [] (Internal, Void :-> Void) [] es
            where f_name = qualify (sName smName StateAny, mangleEv EventExit)
                  es = [Cases (Value $ Var stateVar) cases []]
                  cases = [(st_id s, [ExprS $ FunCall (qualify (s, mangleEv EventExit)) []]) | (State s) <- ss]
