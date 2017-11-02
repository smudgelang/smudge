-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.DeclaredStateNames (
    DeclaredStateNames
) where

import Grammars.Smudge (StateMachine(..), WholeState, State(..))
import Parsers.Id (at)
import Model (TaggedName, disqualifyTag)
import Semantics.Semantic (Passable(..), Severity(..), Fault(..))

import Data.Set (Set, singleton, fromList, toList, (\\))

data DeclaredStateNames = DeclaredStateNames (Set (State TaggedName)) (Set (State TaggedName))
instance Monoid DeclaredStateNames where
    mempty = DeclaredStateNames mempty mempty
    mappend (DeclaredStateNames sf st) (DeclaredStateNames sf' st') =
        DeclaredStateNames (mappend sf sf') (mappend st st')

instance Passable DeclaredStateNames where
    type Representation DeclaredStateNames = [WholeState TaggedName]
    accumulate (s, _, _, hs, _) = mappend $ DeclaredStateNames (singleton s) (fromList [s' | (_, _, s'@(State _)) <- hs])
    test (StateMachine sm_name, _) (DeclaredStateNames sf st) =
        case toList (st \\ sf) of
        [] -> []
        ss -> [Fault ERROR (at name) $ (disqualifyTag sm_name) ++ ": State name not declared: " ++
               disqualifyTag name | State name <- ss]
