-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.NoTransientStateCycles (
    NoTransientStateCycles
) where

import Grammars.Smudge (StateMachine(..), State(..), Event(EventEnter))
import Model (EnterExitState(..), Happening(..), disqualifyTag)
import Parsers.Id (at)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))
import Trashcan.Graph (cycles)

import Data.Graph.Inductive.Graph (Graph, Context, (&), lab)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Monoid (Monoid(..))
import Data.List (intercalate)

data (Graph gr) => NoTransientStateCycles gr = NoTransientStateCycles (Gr EnterExitState Happening)
instance (Graph gr) => Monoid (NoTransientStateCycles gr) where
    mempty = NoTransientStateCycles mempty
    mappend (NoTransientStateCycles a) (NoTransientStateCycles b) = NoTransientStateCycles (mappend a b)

instance (Graph gr) => Passable (NoTransientStateCycles gr) where
    type Representation (NoTransientStateCycles gr) = gr EnterExitState Happening
    accumulate = tfilter
    test (StateMachine sm_name, _) (NoTransientStateCycles g) =
        case (cycles g) of
        [] -> []
        cs -> [Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": Transient state cycles are forbidden: " ++
               (intercalate " -> " $ [disqualifyTag name | State name <- [st ees | Just ees <- map (lab g) c]]) | c <- cs]

tfilter :: (Graph gr) => Context EnterExitState Happening -> NoTransientStateCycles gr -> NoTransientStateCycles gr
tfilter (i, n, l, o) (NoTransientStateCycles a) = NoTransientStateCycles ((efs i, n, l, efs o) & a)
    where efs es = [e | e@(Happening EventEnter _ _, _) <- es]
