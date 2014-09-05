module Backends.Backend (
    Backend(..),
) where

import Grammars.Smudge (StateMachine, State, Happening)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> [(StateMachine, Gr State Happening)] -> FilePath -> IO [FilePath]
