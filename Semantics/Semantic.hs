{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.Semantic (
    AbstractFoldable(..),
    Passable(..),
    Severity(..),
    Fault(..),
    fatal,
) where

import Model (TaggedName)
import Grammars.Smudge (StateMachine)
import Parsers.Id (Location)

import Data.Graph.Inductive.Graph (Graph, Context, ufold)
import Data.Monoid (Monoid, mempty)

data Severity = ERROR | BUG
    deriving (Show, Eq, Ord)

type Description = String
data Fault = Fault Severity Location Description

fatal :: Fault -> Bool
fatal (Fault ERROR _ _) = True
fatal (Fault BUG _ _) = True

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

class (AbstractFoldable (Representation p), Monoid p) => Passable p where
    type Representation p :: *

    accumulate :: FoldContext (Representation p) -> p -> p
    test :: (StateMachine TaggedName, Representation p) -> p -> [Fault]

    pass :: (StateMachine TaggedName, Representation p) -> p -> [Fault]
    pass sm@(_, r) dummyp = xtest (afold accumulate mempty r) dummyp
        where
            xtest :: p -> p -> [Fault]
            xtest = const . (test sm)
