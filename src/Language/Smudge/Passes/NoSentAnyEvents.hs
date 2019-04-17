-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoSentAnyEvents (
    NoSentAnyEvents
) where

import Language.Smudge.Grammar (
    StateMachine(StateMachine),
    WholeState,
    )
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Semigroup (Semigroup)

newtype NoSentAnyEvents = NoSentAnyEvents [TaggedName]
    deriving (Semigroup, Monoid)

instance Passable NoSentAnyEvents where
    type Representation NoSentAnyEvents = [WholeState TaggedName]
    accumulate (_, _, en, hs, ex) = mappend $ NoSentAnyEvents $ filter (("_" ==) . disqualifyTag) fs
        where fs = map fst $ en ++ concatMap (\(_, ses, _) -> ses) hs ++ ex
    test                         _ (NoSentAnyEvents []) = []
    test (StateMachine sm_name, _) (NoSentAnyEvents es) =
        [Fault ERROR (at ev) $ disqualifyTag sm_name ++ ": Any-event forbidden as side effect" | ev <- es]
