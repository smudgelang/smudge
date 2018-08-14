-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.DeclaredStateNames (
    DeclaredStateNames
) where

import Language.Smudge.Grammar (StateMachine(..), WholeState, State(..))
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Data.Semigroup (Semigroup(..))
import Data.Set (Set, singleton, fromList, toList, (\\))

data DeclaredStateNames = DeclaredStateNames (Set (State TaggedName)) (Set (State TaggedName))
instance Semigroup DeclaredStateNames where
    (DeclaredStateNames sf st) <> (DeclaredStateNames sf' st') =
        DeclaredStateNames (sf <> sf') (st <> st')
instance Monoid DeclaredStateNames where
    mempty = DeclaredStateNames mempty mempty
    mappend = (<>)

instance Passable DeclaredStateNames where
    type Representation DeclaredStateNames = [WholeState TaggedName]
    accumulate (s, _, _, hs, _) = mappend $ DeclaredStateNames (singleton s) (fromList [s' | (_, _, s'@(State _)) <- hs])
    test (StateMachine sm_name, _) (DeclaredStateNames sf st) =
        case toList (st \\ sf) of
        [] -> []
        ss -> [Fault ERROR (at name) $ (disqualifyTag sm_name) ++ ": State name not declared: " ++
               disqualifyTag name | State name <- ss]
