-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoTransientAnyState (
    NoTransientAnyState
) where

import Language.Smudge.Grammar (StateMachine(..), State(..), Event(EventEnter))
import Language.Smudge.Semantics.Model (EnterExitState(..), Happening(..), disqualifyTag)
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Graph.Inductive.Graph (Graph, lab, Node)
import Data.Monoid (Monoid(..))
import Data.List (intercalate)
import Data.Semigroup (Semigroup(..))

data (Graph gr) => NoTransientAnyState gr = NoTransientAnyState [Node] [(Node, Node)]
instance (Graph gr) => Semigroup (NoTransientAnyState gr) where
    (NoTransientAnyState a1 b1) <> (NoTransientAnyState a2 b2) = NoTransientAnyState (a1 <> a2) (b1 <> b2)
instance (Graph gr) => Monoid (NoTransientAnyState gr) where
    mempty = NoTransientAnyState mempty mempty
    mappend = (<>)

instance (Graph gr) => Passable (NoTransientAnyState gr) where
    type Representation (NoTransientAnyState gr) = gr EnterExitState Happening
    accumulate (_, n , EnterExitState {st = StateAny}, o) a = mappend (NoTransientAnyState [n] [(n, n') | (Happening EventEnter _ _, n') <- o]) a
    accumulate (i, n',                              _, _) a = mappend (NoTransientAnyState []  [(n, n') | (Happening EventEnter _ _, n ) <- i]) a
    test (StateMachine sm_name, g) (NoTransientAnyState ns ls) =
        case map snd $ filter ((`elem` ns) . fst) ls of
        []  -> []
        ns' -> [Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": Transient any state is meaningless; no way to transition to state: " ++
                (intercalate ", " $ [disqualifyTag name | Just (EnterExitState {st = State name}) <- [lab g n']]) | n' <- ns']
