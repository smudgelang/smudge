module Backends.Backend (
    Backend(..),
    EventAndSideEffects(..)
) where

import Grammar

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

data EventAndSideEffects = EventAndSideEffects Event [SideEffect]
    deriving (Show, Eq, Ord)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Gr State EventAndSideEffects -> FilePath -> IO FilePath
