module Semantics.NoTransientStateCycles (
    NoTransientStateCycles
) where

import Grammars.Smudge (State, Event(EventEnter))
import Model (EnterExitState, Happening(..))
import Semantics.Semantic (Passable(..))
import Trashcan.Graph (cycles)

import Data.Graph.Inductive.Graph (Context, (&))
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Monoid (Monoid(..))

data NoTransientStateCycles = NoTransientStateCycles (Gr EnterExitState Happening)
instance Monoid NoTransientStateCycles where
    mempty = NoTransientStateCycles mempty
    mappend (NoTransientStateCycles a) (NoTransientStateCycles b) = NoTransientStateCycles (mappend a b)

instance Passable NoTransientStateCycles where
    accumulate = tfilter
    test (NoTransientStateCycles g) = null $ cycles g

tfilter :: Context EnterExitState Happening -> NoTransientStateCycles -> NoTransientStateCycles
tfilter (i, n, l, o) (NoTransientStateCycles a) = NoTransientStateCycles ((efs i, n, l, efs o) & a)
    where efs es = [e | e@(Hustle EventEnter _, _) <- es]
