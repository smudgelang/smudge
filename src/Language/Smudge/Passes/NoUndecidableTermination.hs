-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE TypeFamilies #-}

module Language.Smudge.Passes.NoUndecidableTermination (
    NoUndecidableTermination
) where

import Language.Smudge.Grammar (StateMachine(..), State(State), Event(Event), Function(FuncEvent))
import Language.Smudge.Semantics.Model (TaggedName, disqualifyTag)
import Language.Smudge.Semantics.Operation (BasicBlock(..))
import Language.Smudge.Parsers.Id (at)
import Language.Smudge.Passes.Passes (Passable(..), Severity(..), Fault(..))
import Language.Smudge.Passes.NoDecidableNontermination (NoDecidableNontermination)

import Data.Monoid (Monoid(..))

data NoUndecidableTermination = NoUndecidableTermination [(NoDecidableNontermination, (State TaggedName, Event TaggedName))]
instance Semigroup NoUndecidableTermination where
    (NoUndecidableTermination as) <> (NoUndecidableTermination bs) =
        NoUndecidableTermination $ as <> bs
instance Monoid NoUndecidableTermination where
    mempty = NoUndecidableTermination []
    mappend = (<>)

instance Passable NoUndecidableTermination where
    type Representation NoUndecidableTermination = [((State TaggedName, Event TaggedName), BasicBlock)]
    accumulate   ((_, _), BasicBlock {full = ([], _,   _)}) a = a
    accumulate   ((s, e), BasicBlock {full = ( _, _,  [])}) a = a
    accumulate b@((s, e), BasicBlock {full = (es, _, ses)}) a =
        let selfEv (_, (FuncEvent (_, e'))) = e' `elem` (e:es)
            selfEv _ = False
        in if not $ all selfEv ses then a
           else NoUndecidableTermination [(accumulate b (mempty :: NoDecidableNontermination), (s, e))] <> a
    test bs@(StateMachine sm_name, _) (NoUndecidableTermination nts) =
        case map snd $ filter (null . test bs . fst) nts of
        []  -> []
        nts -> [Fault ERROR (at e) $ (disqualifyTag sm_name) ++ ": Undecidable termination is forbidden: state " ++ show (disqualifyTag s) ++
                              " receiving event " ++ show (disqualifyTag e) ++ " when idle is not guaranteed to terminate." | (State s, Event e) <- nts]
