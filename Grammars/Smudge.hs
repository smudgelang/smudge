module Grammars.Smudge (
    Module(..),
    StateMachine(..),
    State(..),
    Event(..),
    SideEffect(..),
    EventHandler,
    StateFlag(..),
    WholeState,
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

type EventHandler = (Event, [SideEffect], State)

data StateFlag = Initial
    deriving (Show, Eq, Ord)

type WholeState = (State, [StateFlag], [SideEffect], [EventHandler], [SideEffect])
