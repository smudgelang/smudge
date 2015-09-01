module Semantics.OneInitialState (
    OneInitialState
) where

import Grammars.Smudge (State(..))
import Model (EnterExitState(..), Happening)
import Semantics.Semantic (Passable(..))

import Data.Graph.Inductive.Graph (Adj)
import Data.Monoid (Monoid(..))

data OneInitialState = OneInitialState (Adj Happening) (Adj Happening)
instance Monoid OneInitialState where
    mempty = OneInitialState mempty mempty
    mappend (OneInitialState is os) (OneInitialState is' os') =
        OneInitialState (mappend is is') (mappend os os')

instance Passable OneInitialState where
    accumulate (i, _, EnterExitState {st = StateEntry}, o) a = mappend (OneInitialState i o) a
    accumulate                                           _ a = a
    test (OneInitialState is os) = (length is == 0) && (length os == 1)
