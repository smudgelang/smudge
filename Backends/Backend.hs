module Backends.Backend (
    Backend(..),
) where

import Grammars.Smudge (StateMachine, State, SideEffect)
import Model (QualifiedName, Happening, EnterExitState, SymbolTable)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

type Transition = (Maybe (SideEffect QualifiedName), Maybe (SideEffect QualifiedName))

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> ([(StateMachine QualifiedName, Gr EnterExitState Happening)], SymbolTable) -> FilePath -> IO [FilePath]
