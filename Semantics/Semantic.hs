{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.Semantic (
    Passable(..),
    Severity(..),
    Fault(..),
) where

import Model (TaggedName)
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

class AbstractFoldable f where
    type FoldContext f :: *
    afold :: (FoldContext f -> b -> b) -> b -> f -> b

instance (Graph gr) => AbstractFoldable (gr a b) where
    type FoldContext (gr a b) = Context a b
    afold = ufold

instance AbstractFoldable [a] where
    type FoldContext [a] = a
    afold = foldr

class (AbstractFoldable f, Monoid p) => Passable f p where
    accumulate :: f -> FoldContext f -> p -> p
    test :: (StateMachine TaggedName, f) -> p -> [Fault]

    pass :: (StateMachine TaggedName, f) -> p -> [Fault]
    pass sm@(_, f) dummyp = xtest (afold (accumulate dummyf) mempty f) dummyp
        where
            xtest :: p -> p -> [Fault]
            xtest = const . (test sm)
            dummyf = undefined :: f
