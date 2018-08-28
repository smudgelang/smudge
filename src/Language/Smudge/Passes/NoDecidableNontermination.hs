-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoDecidableNontermination (
    NoDecidableNontermination
) where

import Language.Smudge.Grammar (StateMachine(..), State(State), Event(Event), Function(FuncEvent))
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Semantics.Operation (BasicBlock(..))
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))

import Control.Arrow (first)
import Data.Maybe (mapMaybe, isJust)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

data NoDecidableNontermination = NoDecidableNontermination [(State TaggedName, Event TaggedName)]
instance Semigroup NoDecidableNontermination where
    (NoDecidableNontermination as) <> (NoDecidableNontermination bs) =
        NoDecidableNontermination $ as <> bs
instance Monoid NoDecidableNontermination where
    mempty = NoDecidableNontermination []
    mappend = (<>)

data Pat a = PatSym a
           | PatCat (Pat a) (Pat a)
           | PatAlt (Pat a) (Pat a)
           | PatStar (Pat a)

match :: Eq a => Pat a -> [a] -> Maybe ([a], [a])
match = go (const Just) Nothing
    where go s f     (PatSym x) xs = if not (null xs) && x == head xs then s f $ splitAt 1 xs else f
          go s f (PatCat p1 p2) xs = go (\n1 (m1, ys) -> go (\n2 (m2, zs) -> s n2 (m1 ++ m2, zs)) n1 p2 ys) f p1 xs
          go s f (PatAlt p1 p2) xs = go s (go s f p2 xs) p1 xs
          go s f p@(PatStar p1) xs = go s (s f ([], xs)) (PatCat p1 p) xs

instance Passable NoDecidableNontermination where
    type Representation NoDecidableNontermination = [((State TaggedName, Event TaggedName), BasicBlock)]
    accumulate ((_, _), BasicBlock {safe = ([],  _,   _)}) a = a
    accumulate ((s, e), BasicBlock {safe = (es, s', ses)}) a =
        let evtOf (_, (FuncEvent (_, e))) = Just e
            evtOf _ = Nothing
            syms@(sym:_) = map PatSym es
            -- zero or more repititions of all seen events, followed by one or more of the first event
            pat = PatCat (PatStar $ foldr1 PatCat syms) $ PatCat sym $ PatStar sym
            (m, rest) = first (match pat . mapMaybe evtOf) $ span (isJust . evtOf) ses
        in case (s == s', rest, m) of
        (True, [], Just (_, [])) -> NoDecidableNontermination [(s, e)] <> a
        otherwise -> a
    test (StateMachine sm_name, _) (NoDecidableNontermination nts) =
        [Fault ERROR (at e) $ (disqualifyTag sm_name) ++ ": Irrefutable cycles are forbidden: state " ++ show (disqualifyTag s) ++
                              " receiving event " ++ show (disqualifyTag e) ++ " when idle is guaranteed not to terminate." | (State s, Event e) <- nts]
