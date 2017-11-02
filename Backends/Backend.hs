-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

module Backends.Backend (
    Backend(..),
    Config(..),
    defaultConfig
) where

import Grammars.Smudge (StateMachine, State, SideEffect)
import Model (QualifiedName, TaggedName, Happening, EnterExitState)
import Semantics.Semantic (Fault)
import Semantics.Solver (SymbolTable)
import Semantics.Alias (Alias)

import Control.Monad.Trans.Except (ExceptT)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Trashcan.GetOpt (OptDescr)
import System.FilePath (FilePath)

data Config = Config {
    debug :: Bool,
    logEvent :: Bool
}

defaultConfig = Config { debug=True, logEvent=False }

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Config -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable) -> FilePath -> ExceptT Fault IO [FilePath]
