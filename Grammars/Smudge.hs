module Grammars.Smudge (
    Annotated(..),
    Module(..),
    StateMachineDeclarator(..),
    StateMachine(..),
    State(..),
    Event(..),
    SideEffect(..),
    EventHandler,
    StateFlag(..),
    WholeState,
) where

import Text.ParserCombinators.Parsec (SourcePos) -- Sorry.

data Annotated a = Annotated SourcePos a
    deriving (Show, Eq, Ord)

data Module = Module String [StateMachine]

data StateMachineDeclarator = StateMachineDeclarator String | StateMachineSame
    deriving (Show, Eq, Ord)

type StateMachine = Annotated StateMachineDeclarator

data State = State String | StateAny | StateSame | StateEntry
    deriving (Show, Eq, Ord)

data Event = Event String | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

data SideEffect = FuncVoid String | FuncEvent String (StateMachineDeclarator, Event) | FuncDefault (StateMachineDeclarator, Event)
    deriving (Show, Eq, Ord)

type EventHandler = (Event, [SideEffect], State)

data StateFlag = Initial
    deriving (Show, Eq, Ord)

type WholeState = (State, [StateFlag], [SideEffect], [EventHandler], [SideEffect])
