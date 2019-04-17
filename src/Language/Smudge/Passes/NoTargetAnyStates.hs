-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoTargetAnyStates (
    NoTargetAnyStates
) where

import Language.Smudge.Grammar (
    StateMachine(StateMachine),
    WholeState,
    State(State),
    )
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Semigroup (Semigroup)

newtype NoTargetAnyStates = NoTargetAnyStates [TaggedName]
    deriving (Semigroup, Monoid)

instance Passable NoTargetAnyStates where
    type Representation NoTargetAnyStates = [WholeState TaggedName]
    accumulate (_, _, _, hs, _) = mappend $ NoTargetAnyStates [st | (_, _, State st) <- hs, "_" == disqualifyTag st]
    test                         _ (NoTargetAnyStates []) = []
    test (StateMachine sm_name, _) (NoTargetAnyStates ss) =
        [Fault ERROR (at st) $ disqualifyTag sm_name ++ ": Any-state forbidden in state transition" | st <- ss]
