-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

module Language.Smudge.Grammar (
    Module(..),
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

data Module a = Module String [StateMachine a]

data StateMachine a = StateMachine a | StateMachineSame
    deriving (Show, Eq, Ord)

instance Functor StateMachine where
    fmap f (StateMachine a) = StateMachine (f a)
    fmap _ StateMachineSame = StateMachineSame

data State a = State a | StateAny | StateSame | StateEntry
    deriving (Show, Eq, Ord)

instance Functor State where
    fmap f (State a)  = State (f a)
    fmap _ StateAny   = StateAny
    fmap _ StateSame  = StateSame
    fmap _ StateEntry = StateEntry

data Event a = Event a | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

instance Functor Event where
    fmap f (Event a)  = Event (f a)
    fmap _ EventAny   = EventAny
    fmap _ EventEnter = EventEnter
    fmap _ EventExit  = EventExit

type QEvent a = (StateMachine a, Event a)

data Function a = FuncVoid | FuncTyped (QEvent a) | FuncEvent (QEvent a)
    deriving (Show, Eq, Ord)

type SideEffect a = (a, Function a)

type EventHandler a = (Event a, [SideEffect a], State a)

data StateFlag = Initial
    deriving (Show, Eq, Ord)

type WholeState a = (State a, [StateFlag], [SideEffect a], [EventHandler a], [SideEffect a])
