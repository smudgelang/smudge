module Semantics.Semantic (
    Passable(..),
    pass
) where

import Model (EnterExitState, Happening)

import Data.Graph.Inductive.Graph (Graph, Context, ufold)
import Data.Monoid (Monoid, mempty)

class Monoid a => Passable a where
    accumulate :: Context EnterExitState Happening -> a -> a
    test :: a -> Bool

pass :: (Graph gr, Passable a) => gr EnterExitState Happening -> a -> Bool
pass g b = xtest (ufold accumulate mempty g) b
    where
        xtest :: Passable a => a -> a -> Bool
        xtest = const . test
