module Model (
    EnterExitState(..),
    HappeningFlag(..),
    Happening(..),
    smToGraph,
) where

import Grammars.Smudge (StateMachine, State(..), Event, SideEffect, WholeState)

import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromList, (!))

type EnterExitState = ([SideEffect], State, [SideEffect])

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
    mkGraph [s | s <- zip [1..] (map getEeState ss)] es
    where
        getEeState (s, _, en, _, ex) = (en, s, ex)
        getState (s, _, _, _, _) = s
        sn :: Map State Node
        sn = fromList [s | s <- zip (map getState ss) [1..]]
        mkEdge :: State -> State -> Happening -> (Node, Node, Happening)
        mkEdge s s'' eses = (sn ! s, sn ! s'', eses)
        es = [ese | ese <- concat $ map f ss]
            where
                f :: WholeState -> [(Node, Node, Happening)]
                f (s, _, _, es, _) = map g es
                    where
                        g :: (Event, [SideEffect], State) -> (Node, Node, Happening)
                        g (e, ses, s') =
                            let (e', s'') = case s' of
                                    StateSame -> (Happening e ses [NoTransition], s)
                                    otherwise -> (Happening e ses [], s')
                            in mkEdge s s'' e'
