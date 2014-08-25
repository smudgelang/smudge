module Backends.Backend (
    Backend(..),
) where

import Grammars.Smudge (State, EventAndSideEffects)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Gr State EventAndSideEffects -> FilePath -> IO FilePath
