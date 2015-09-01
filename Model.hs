{-# LANGUAGE RecordWildCards #-}

module Model (
    EnterExitState(..),
    HappeningFlag(..),
    Happening(..),
    smToGraph,
) where

import Grammars.Smudge (StateMachine, State(..), Event(..), SideEffect, StateFlag(..), WholeState)

import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromList, (!))

data EnterExitState = EnterExitState {
        en :: [SideEffect],
        st :: State,
        ex :: [SideEffect]
    } deriving (Show, Eq, Ord)

data HappeningFlag = NoTransition
    deriving (Show, Eq, Ord)

data Happening = Happening {
        event       :: Event,
        sideEffects :: [SideEffect],
        flags       :: [HappeningFlag]
    } deriving (Show, Eq, Ord)

smToGraph :: (StateMachine, [WholeState]) ->
                 Gr EnterExitState Happening
smToGraph (sm, ss) =
    -- Graph.mkGraph :: [(Node, node)] -> [(Node, Node, edge)] -> gr node edge
    mkGraph [s | s <- zip [1..] (EnterExitState [] StateEntry [] : map getEeState ss)] es
    where
        getEeState (st, _, en, _, ex) = EnterExitState {..}
        getState (s, _, _, _, _) = s
        sn :: Map State Node
        sn = fromList [s | s <- zip (map getState ss) [2..]]
        mkEdge :: State -> State -> Happening -> (Node, Node, Happening)
        mkEdge s s'' eses = (sn ! s, sn ! s'', eses)
        es = [ese | ese <- concat $ map f ss]
            where
                f :: WholeState -> [(Node, Node, Happening)]
                f (s, fs, _, es, _) | elem Initial fs = (1, sn ! s, Happening EventEnter [] []) : f (s, [], [], es, [])
                f (s,  _, _, es, _) = map g es
                    where
                        g :: (Event, [SideEffect], State) -> (Node, Node, Happening)
                        g (e, ses, s') =
                            let (e', s'') = case s' of
                                    StateSame -> (Happening e ses [NoTransition], s)
                                    otherwise -> (Happening e ses [], s')
                            in mkEdge s s'' e'
