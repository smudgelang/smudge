-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoAnyEnterExit (
    NoAnyEnterExit
) where

import Language.Smudge.Grammar (StateMachine(..), State(StateAny), SideEffect(..))
import Language.Smudge.Semantics.Model (EnterExitState(..), Happening, TaggedName, disqualifyTag)
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Graph.Inductive.Graph (Graph)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.List (intercalate)

data (Graph gr) => NoAnyEnterExit gr = NoAnyEnterExit [SideEffect TaggedName]
instance (Graph gr) => Semigroup (NoAnyEnterExit gr) where
    (NoAnyEnterExit a) <> (NoAnyEnterExit b) = NoAnyEnterExit (a <> b)
instance (Graph gr) => Monoid (NoAnyEnterExit gr) where
    mempty = NoAnyEnterExit mempty
    mappend = (<>)

instance (Graph gr) => Passable (NoAnyEnterExit gr) where
    type Representation (NoAnyEnterExit gr) = gr EnterExitState Happening
    accumulate (_, n , EnterExitState []        _ [], o) a = a
    accumulate (_, n , EnterExitState en StateAny ex, o) a = mappend (NoAnyEnterExit $ en ++ ex) a
    accumulate                                         _ a = a
    test (StateMachine sm_name, g) (NoAnyEnterExit ses) =
        case map fst ses of
        [] -> []
        xs -> [Fault ERROR (at sm_name) $ (disqualifyTag sm_name) ++ ": Any-state enter and exit functions are forbidden: " ++
               (intercalate ", " $ map disqualifyTag xs)]
