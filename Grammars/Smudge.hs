module Grammars.Smudge (
    StateMachine(..),
    State(..),
    Event(..),
    SideEffect(..),
    EventAndSideEffects(..),
) where

data StateMachine = StateMachine String | StateMachineSame
    deriving (Show, Eq, Ord)

data State = State String | StateAny | StateSame
    deriving (Show, Eq, Ord)

data Event = Event String | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

data SideEffect = FuncVoid String | FuncEvent String (StateMachine, Event) | FuncDefault (StateMachine, Event)
    deriving (Show, Eq, Ord)

data EventAndSideEffects = EventAndSideEffects Event [SideEffect]
    deriving (Show, Eq, Ord)
