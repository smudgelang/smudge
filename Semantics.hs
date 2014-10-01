module Semantics (
    make_passes
) where

import Model (EnterExitState, Happening)
import Semantics.Semantic (pass)
import Semantics.NoTransientStateCycles (NoTransientStateCycles)
import Semantics.UniqueStateNames (UniqueStateNames)

import Data.Graph.Inductive.Graph (Graph)

make_passes :: Graph gr => gr EnterExitState Happening -> Bool
make_passes g = and [pass g (undefined :: UniqueStateNames),
                     pass g (undefined :: NoTransientStateCycles)]
