module Semantics (
    make_passes
) where

import Model (EnterExitState, Happening)
import Grammars.Smudge (StateMachine)
import Semantics.Semantic (pass, Fault)
import Semantics.NoTransientStateCycles (NoTransientStateCycles)
import Semantics.UniqueStateNames (UniqueStateNames)
import Semantics.OneInitialState (OneInitialState)

import Data.Graph.Inductive.Graph (Graph)

make_passes :: Graph gr => (StateMachine, gr EnterExitState Happening) -> [Fault]
make_passes g = concat [pass g (undefined :: UniqueStateNames),
                        pass g (undefined :: OneInitialState),
                        pass g (undefined :: NoTransientStateCycles)]
