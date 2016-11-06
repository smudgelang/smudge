{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.NoTransientStateCycles (
    NoTransientStateCycles
) where

import Grammars.Smudge (State(..), Event(EventEnter), Annotated(..), StateMachineDeclarator(..))
import Model (EnterExitState(..), Happening(..), disqualifyTag)
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
    test (Annotated pos (StateMachineDeclarator sm_name), _) (NoTransientStateCycles g) =
        case (cycles g) of
        [] -> []
        cs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": Transient state cycles are forbidden: " ++
               (intercalate " -> " $ [disqualifyTag name | State name <- [st ees | Just ees <- map (lab g) c]]) | c <- cs]

tfilter :: (Graph gr) => Context EnterExitState Happening -> NoTransientStateCycles gr -> NoTransientStateCycles gr
tfilter (i, n, l, o) (NoTransientStateCycles a) = NoTransientStateCycles ((efs i, n, l, efs o) & a)
    where efs es = [e | e@(Happening EventEnter _ _, _) <- es]
