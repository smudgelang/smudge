module Semantics.UniqueStateNames (
    UniqueStateNames
) where

import Grammars.Smudge (State)
import Model (EnterExitState(..))
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
    accumulate (_, _, ees, _) (UniqueStateNames sl ss) = UniqueStateNames (st ees:sl) (insert (st ees) ss)
    test (UniqueStateNames sl ss) = (sort sl) == (sort (toList ss))
