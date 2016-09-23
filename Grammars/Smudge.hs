module Grammars.Smudge (
    Name,
    Annotated(..),
    Module(..),
    StateMachineDeclarator(..),
    StateMachine(..),
    State(..),
    Event(..),
    QEvent,
    Function(..),
    SideEffect(..),
    EventHandler,
    StateFlag(..),
    WholeState,
) where

import Text.ParserCombinators.Parsec (SourcePos) -- Sorry.

type Name = String

data Annotated a = Annotated SourcePos a
    deriving (Show, Eq, Ord)

data Module a = Module String [StateMachine a]

data StateMachineDeclarator a = StateMachineDeclarator a | StateMachineSame
    deriving (Show, Eq, Ord)

type StateMachine a = Annotated (StateMachineDeclarator a)

data State a = State a | StateAny | StateSame | StateEntry
    deriving (Show, Eq, Ord)

data Event a = Event a | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

type QEvent a = (StateMachineDeclarator a, Event a)

data Function a = FuncVoid | FuncTyped (QEvent a) | FuncEvent (QEvent a)
    deriving (Show, Eq, Ord)

type SideEffect a = (a, Function a)

type EventHandler a = (Event a, [SideEffect a], State a)

data StateFlag = Initial
    deriving (Show, Eq, Ord)

type WholeState a = (State a, [StateFlag], [SideEffect a], [EventHandler a], [SideEffect a])
