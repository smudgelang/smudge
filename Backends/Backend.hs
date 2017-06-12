module Backends.Backend (
    Backend(..),
    Config(..),
    defaultConfig
) where

import Grammars.Smudge (StateMachine, State, SideEffect)
import Model (QualifiedName, TaggedName, Happening, EnterExitState)
import Semantics.Solver (SymbolTable)
import Semantics.Alias (Alias)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

data Config = Config {
    debug :: Bool,
    logEvent :: Bool
}

defaultConfig = Config { debug=True, logEvent=False }

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Config -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable) -> FilePath -> IO [FilePath]
