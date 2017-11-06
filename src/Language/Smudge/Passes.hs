-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

module Language.Smudge.Passes (
    make_passes,
    name_passes,
    type_passes,
) where

import Language.Smudge.Semantics.Model (EnterExitState, Happening, TaggedName)
import Language.Smudge.Semantics.Solver (SymbolTable)
import Language.Smudge.Grammar (StateMachine, WholeState)
import Language.Smudge.Passes.Passes (pass, Fault)
import Language.Smudge.Passes.NoTransientStateCycles (NoTransientStateCycles)
import Language.Smudge.Passes.UniqueStateNames (UniqueStateNames)
import Language.Smudge.Passes.OneInitialState (OneInitialState)
import Language.Smudge.Passes.DeclaredStateNames (DeclaredStateNames)
import Language.Smudge.Passes.DeclaredEventNames (DeclaredEventNames)
import Language.Smudge.Passes.UninstantiableTypes (UninstantiableTypes)

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
