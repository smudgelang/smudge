module Semantics.ValidDestinationStates (
    ValidDestinationStates
) where

import Grammars.Smudge (State)
import Semantics.Semantic (Passable(..))

import Data.Set (Set, isSubsetOf)
import Data.Monoid (Monoid(..))

data ValidDestinationStates = ValidDestinationStates [State] [State]
instance Monoid ValidDestinationStates where
    mempty = ValidDestinationStates mempty mempty
    mappend (ValidDestinationStates a b)) (ValidDestinationStates c d) =
        ValidDestinationStates (mappend a c) (mappend b d)

instance Passable ValidDestinationStates where
    accumulate (ein@[(b, Node)], _, a, eout@[(b, Node)]) (ValidDestinationStates (a, b)) = ValidDestinationStates (a, b)
    test (ValidDestinationStates (a, b)) = (fromList b) `isSubsetOf` (fromList a)
