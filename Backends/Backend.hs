module Backends.Backend (
    Backend(..),
) where

import Grammars.Smudge (StateMachine, State, Happening, WholeState, SideEffect)

import Data.Graph.Inductive.PatriciaTree (Gr)
import System.Console.GetOpt (OptDescr)
import System.FilePath (FilePath)

type Transition = (Maybe SideEffect, Maybe SideEffect)

class Backend a where
    options :: (String, [OptDescr a])
    generate :: [a] -> [(StateMachine, Gr WholeState Happening)] -> FilePath -> IO FilePath
