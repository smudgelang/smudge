module Semantics (
    make_passes,
    name_passes,
) where

import Model (EnterExitState, Happening, TaggedName)
import Grammars.Smudge (StateMachine, WholeState)
import Semantics.Semantic (pass, Fault)
import Semantics.NoTransientStateCycles (NoTransientStateCycles)
import Semantics.UniqueStateNames (UniqueStateNames)
import Semantics.OneInitialState (OneInitialState)
import Semantics.DeclaredStateNames (DeclaredStateNames)
import Semantics.DeclaredEventNames (DeclaredEventNames)

import Data.Graph.Inductive.Graph (Graph)

make_passes :: Graph gr => (StateMachine TaggedName, gr EnterExitState Happening) -> [Fault]
make_passes g = concat [pass g (undefined :: OneInitialState),
                        pass g (undefined :: NoTransientStateCycles)]

name_passes :: (StateMachine TaggedName, [WholeState TaggedName]) -> [Fault]
name_passes sm = concat [pass sm (undefined :: DeclaredStateNames),
                         pass sm (undefined :: UniqueStateNames),
                         pass sm (undefined :: DeclaredEventNames)]
