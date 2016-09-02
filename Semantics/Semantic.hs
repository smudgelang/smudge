module Semantics.Semantic (
    Passable(..),
    Severity(..),
    Fault(..),
    pass
) where

import Model (EnterExitState, Happening)
import Grammars.Smudge (StateMachine)

import Text.ParserCombinators.Parsec (SourcePos) -- Sorry.
import Data.Graph.Inductive.Graph (Graph, Context, ufold)
import Data.Monoid (Monoid, mempty)

data Severity = ERROR | BUG
    deriving (Show, Eq, Ord)

type Location = SourcePos
type Description = String
data Fault = Fault Severity Location Description

instance Show Fault where
    show (Fault s l d) = show s ++ " at " ++ show l ++ ":\n" ++ d

class Monoid a => Passable a where
    accumulate :: Context EnterExitState Happening -> a -> a
    test :: (Graph gr) => (StateMachine, gr EnterExitState Happening) -> a -> [Fault]

pass :: (Graph gr, Passable a) => (StateMachine, gr EnterExitState Happening) -> a -> [Fault]
pass sm@(_, g) b = xtest (ufold accumulate mempty g) b
    where
        xtest :: Passable a => a -> a -> [Fault]
        xtest = const . (test sm)
