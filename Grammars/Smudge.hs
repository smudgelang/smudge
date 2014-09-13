module Grammars.Smudge (
    Module(..),
    StateMachine(..),
    State(..),
    Event(..),
    SideEffect(..),
    WholeState(..),
    EnterExitState(..),
    Happening(..),
) where

data Module = Module String [StateMachine]

data StateMachine = StateMachine String | StateMachineSame
    deriving (Show, Eq, Ord)

data State = State String | StateAny | StateSame
    deriving (Show, Eq, Ord)

data Event = Event String | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

data SideEffect = FuncVoid String | FuncEvent String (StateMachine, Event) | FuncDefault (StateMachine, Event)
    deriving (Show, Eq, Ord)

type WholeState = (State, Maybe [SideEffect], [(Event, [SideEffect], State)], Maybe [SideEffect])

type EnterExitState = (Maybe [SideEffect], State, Maybe [SideEffect])

{- Happenings are events and their lists of side effects. Hustles are
used when there's a state transition (i.e. -(...)->) and Bustles are
used when there isn't (-(...)-). These names are better than
EventAndSideEffects and NoTransitionEventAndSide Effects.  -}
data Happening = Hustle Event [SideEffect] | Bustle Event [SideEffect]
    deriving (Show, Eq, Ord)
