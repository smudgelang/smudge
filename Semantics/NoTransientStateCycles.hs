{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

data NoTransientStateCycles = NoTransientStateCycles (Gr EnterExitState Happening)
instance Monoid NoTransientStateCycles where
    mempty = NoTransientStateCycles mempty
    mappend (NoTransientStateCycles a) (NoTransientStateCycles b) = NoTransientStateCycles (mappend a b)

instance (Graph gr) => Passable (gr EnterExitState Happening) NoTransientStateCycles where
    accumulate _ = tfilter
    test (Annotated pos (StateMachineDeclarator sm_name), _) (NoTransientStateCycles g) =
        case (cycles g) of
        [] -> []
        cs -> [Fault ERROR pos $ (disqualifyTag sm_name) ++ ": Transient state cycles are forbidden: " ++
               (intercalate " -> " $ [disqualifyTag name | State name <- [st ees | Just ees <- map (lab g) c]]) | c <- cs]

tfilter :: Context EnterExitState Happening -> NoTransientStateCycles -> NoTransientStateCycles
tfilter (i, n, l, o) (NoTransientStateCycles a) = NoTransientStateCycles ((efs i, n, l, efs o) & a)
    where efs es = [e | e@(Happening EventEnter _ _, _) <- es]
