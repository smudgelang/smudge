-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

module Language.Smudge.Backends.Backend (
    Backend(..),
    Config(..),
    defaultConfig
) where

import Language.Smudge.Grammar (StateMachine, State, SideEffect, Event)
import Language.Smudge.Semantics.Model (QualifiedName, TaggedName, Happening, EnterExitState)
import Language.Smudge.Semantics.Solver (SymbolTable)
import Language.Smudge.Semantics.Alias (Alias)
import Language.Smudge.Passes.Passes (Fault)

import Control.Monad.Trans.Except (ExceptT)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Set (Set, empty)
import System.Console.GetOpt.Extra (OptDescr)
import System.FilePath (FilePath)

data Config = Config {
    debug :: Bool,
    logEvent :: Set (Event TaggedName),
    logState :: Set (State TaggedName)
}

defaultConfig = Config {
    debug=True,
    logEvent=empty,
    logState=empty
    }

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Config -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable) -> FilePath -> ExceptT Fault IO [FilePath]
