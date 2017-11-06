-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.Passes (
    AbstractFoldable(..),
    Passable(..),
    Severity(..),
    Fault(..),
    fatal,
) where

import Language.Smudge.Semantics.Model (TaggedName)
import Language.Smudge.Grammar (StateMachine)
import Language.Smudge.Parsers.Id (Location)

import Data.Graph.Inductive.Graph (Graph, Context, ufold)
import Data.Monoid (Monoid, mempty)

data Severity = ERROR | BUG
    deriving (Show, Eq, Ord)

type Description = String
data Fault = Fault Severity Location Description
           | RuntimeFault Severity Description

fatalSeverity :: Severity -> Bool
fatalSeverity ERROR = True
fatalSeverity BUG   = True

fatal :: Fault -> Bool
fatal (Fault s _ _) = fatalSeverity s
fatal (RuntimeFault s _) = fatalSeverity s

instance Show Fault where
    show (Fault s l d) = show s ++ " at " ++ show l ++ ":\n" ++ d
    show (RuntimeFault s d) = show s ++ " during execution:\n" ++ d

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
