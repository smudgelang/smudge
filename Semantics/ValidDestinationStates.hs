module Semantics.ValidDestinationStates where (
    Passable(..)
)

import Semantics.Semantic (Passable(..))

import Data.Set (Set, isSubsetOf)

data ValidDestinationStates = ValidDestinationStates ([State], [State])
instance Monoid ValidDestinationStates where
    mempty = ValidDestinationStates ([], [])
    mappend (ValidDestinationStates (a, b)) (ValidDestinationStates (c, d)) = ValidDestinationStates (a ++ c, b ++ d)

instance Passable ValidDestinationStates where
    accumulate ctx (ValidDestinationStates (a, b)) = ValidDestinationStates (a, b)
    test (ValidDestinationStates (a, b)) = (fromList b) `isSubsetOf` (fromList a)
