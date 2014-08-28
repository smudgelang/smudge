module Backends.Backend (
    Backend(..),
) where

import Grammars.Smudge (State, Happening)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> [Gr State Happening] -> FilePath -> IO FilePath
