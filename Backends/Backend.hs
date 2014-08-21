module Backends.Backend (
    Backend(..),
    EventAndSideEffects(..)
) where

import Grammar

import Data.Graph.Inductive.PatriciaTree
import System.Console.GetOpt
import System.FilePath

data EventAndSideEffects = EventAndSideEffects Event [SideEffect]
    deriving (Show, Eq, Ord)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> Gr State EventAndSideEffects -> FilePath -> IO FilePath
