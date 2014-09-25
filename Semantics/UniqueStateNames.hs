module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (State)
import Semantics.Semantic (Passable(..))

import Data.Foldable (toList)
import Data.List (sort)
import Data.Monoid (Monoid(..))
import Data.Set (Set, isSubsetOf, insert)

data UniqueStateNames = UniqueStateNames [State] (Set State)
instance Monoid UniqueStateNames where
    mempty = UniqueStateNames mempty mempty
    mappend (UniqueStateNames sl ss) (UniqueStateNames sl' ss') =
        UniqueStateNames (mappend sl sl') (mappend ss ss')

instance Passable UniqueStateNames where
    accumulate (_, _, (_, s, _), _) (UniqueStateNames sl ss) = UniqueStateNames (s:sl) (insert s ss)
    test (UniqueStateNames sl ss) = (sort sl) == (sort (toList ss))
