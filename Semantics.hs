module Semantics (
    make_passes,
    name_passes,
    type_passes,
) where

import Model (EnterExitState, Happening, TaggedName)
import Grammars.Smudge (StateMachine, WholeState)
import Semantics.Solver (SymbolTable)
import Semantics.Semantic (pass, Fault)
import Semantics.NoTransientStateCycles (NoTransientStateCycles)
import Semantics.UniqueStateNames (UniqueStateNames)
import Semantics.OneInitialState (OneInitialState)
import Semantics.DeclaredStateNames (DeclaredStateNames)
import Semantics.DeclaredEventNames (DeclaredEventNames)
import Semantics.UninstantiableTypes (UninstantiableTypes)

import Data.Graph.Inductive.Graph (Graph)

make_passes :: Graph gr => (StateMachine TaggedName, gr EnterExitState Happening) -> [Fault]
make_passes g = concat [pass g (undefined :: OneInitialState gr),
                        pass g (undefined :: NoTransientStateCycles gr)]

name_passes :: (StateMachine TaggedName, [WholeState TaggedName]) -> [Fault]
name_passes sm = concat [pass sm (undefined :: DeclaredStateNames),
                         pass sm (undefined :: UniqueStateNames),
                         pass sm (undefined :: DeclaredEventNames)]

type_passes :: (StateMachine TaggedName, SymbolTable) -> [Fault]
type_passes st = concat [pass st (undefined :: UninstantiableTypes)]
