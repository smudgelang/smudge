module Model (
    WholeState(..),
    EnterExitState(..),
    Happening(..),
    smToGraph,
) where

import Grammars.Smudge (StateMachine, State(..), Event, SideEffect)

import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map as Map

type WholeState = (State, Maybe [SideEffect], [(Event, [SideEffect], State)], Maybe [SideEffect])

type EnterExitState = (Maybe [SideEffect], State, Maybe [SideEffect])

{- Happenings are events and their lists of side effects. Hustles are
used when there's a state transition (i.e. -(...)->) and Bustles are
used when there isn't (-(...)-). These names are better than
EventAndSideEffects and NoTransitionEventAndSide Effects.  -}
data Happening = Hustle Event [SideEffect] | Bustle Event [SideEffect]
    deriving (Show, Eq, Ord)


smToGraph :: (StateMachine, [WholeState]) ->
                 Gr EnterExitState Happening
smToGraph (sm, ss) =
    -- Graph.mkGraph :: [(Node, node)] -> [(Node, Node, edge)] -> gr node edge
    mkGraph [s | s <- zip [1..] (map getEeState ss)] es
    where
        getEeState (s, en, _, ex) = (en, s, ex)
        getState (s, _, _, _) = s
        sn :: Map.Map State Node
        sn = Map.fromList [s | s <- zip (map getState ss) [1..]]
        mkEdge :: State -> State -> Happening -> (Node, Node, Happening)
        mkEdge s s'' eses = (sn Map.! s, sn Map.! s'', eses)
        es = [ese | ese <- concat $ map f ss]
            where
                f :: WholeState -> [(Node, Node, Happening)]
                f (s, _, es, _) = map g es
                    where
                        g :: (Event, [SideEffect], State) -> (Node, Node, Happening)
                        g (e, ses, s') =
                            let (e', s'') = case s' of
                                    StateSame -> (Bustle e ses, s)
                                    otherwise -> (Hustle e ses, s')
                            in mkEdge s s'' e'
