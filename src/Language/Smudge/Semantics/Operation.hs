-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE NamedFieldPuns #-}
module Language.Smudge.Semantics.Operation (
    handlers,
    finalStates,
    BasicBlock,
    basicBlocks,
) where

import Language.Smudge.Grammar (
  State(..),
  Event(..),
  SideEffect(..),
  Function(..),
  )
import Language.Smudge.Semantics.Model (
  EnterExitState(..),
  Happening(..),
  HappeningFlag(..),
  TaggedName,
  )

import Data.Graph.Inductive.Graph (
  Graph,
  Node,
  labNodes,
  labEdges,
  nodes,
  out,
  lab,
  edgeLabel,
  )
import Data.List (nub, (\\))
import Data.Map (
  Map,
  (!),
  foldMapWithKey,
  insert,
  )
import qualified Data.Map as Map(map, lookup)
import Data.Sequence (
  Seq,
  ViewL(EmptyL, (:<)),
  viewl,
  (|>),
  (><),
  )
import qualified Data.Sequence as Seq(null)
import Control.Monad (guard)
import GHC.Exts (fromList, toList)

-- The `precedence` function enforces the precedence order for events:
--
-- state | event
-- =============
-- named | named
-- any   | named
-- named | any
-- any   | any
--
-- This could be done with listToMaybe, but it's less explicit.
precedence :: [a] -> [a] -> [a] -> [a] -> Maybe a
precedence [namednamed] _          _          _        = Just namednamed
precedence _            [anynamed] _          _        = Just anynamed
precedence _            _          [namedany] _        = Just namedany
precedence _            _          _          [anyany] = Just anyany
precedence _            _          _          _        = Nothing

handlers :: Graph gr => Event TaggedName -> gr EnterExitState Happening -> Map (State TaggedName) (Maybe (State TaggedName, Event TaggedName))
handlers e g = allHandlers g ! e

allHandlers :: Graph gr => gr EnterExitState Happening -> Map (Event TaggedName) (Map (State TaggedName) (Maybe (State TaggedName, Event TaggedName)))
allHandlers g = fromList $
    do  e <- nub $ map (event . thrd) events
        let all_evt_handlers = all_handlers e
        let all_any_handlers = all_handlers EventAny
        let named_named = [h | h@(State _, _) <- all_evt_handlers]
        let any_named = [h | h@(StateAny, _) <- all_evt_handlers]
        let named_any = [h | h@(State _, _) <- all_any_handlers]
        let any_any = [h | h@(StateAny, _) <- all_any_handlers]
        return $ (,) e $ fromList $
            do  (_, EnterExitState {st=st@(State _)}) <- states
                let st_named = filter ((== st) . fst) named_named
                let st_any = filter ((== st) . fst) named_any
                return (st, precedence st_named any_named st_any any_any)
    where
        thrd (_, _, t) = t
        states = labNodes g
        events = labEdges g
        all_handlers e =
            do  (n, ees) <- states
                (_, _, h) <- out g n
                guard $ event h == e
                return (st ees, e)

finalStates :: Graph gr => gr EnterExitState Happening -> [Node]
finalStates g = nodes g \\
    do  (n, EnterExitState {st=st@(State _)}) <- states
        (e, Just (st', ev')) <- toList $ Map.map (! st) ahs
        guard $ canTransition e
        (m, EnterExitState {st=st''}) <- states
        guard $ st' == st''
        (_, _, Happening {flags}) <- out g m
        guard $ not $ elem NoTransition flags
        return n
    where
        ahs = allHandlers g
        states = labNodes g
        canTransition EventEnter = True
        canTransition (Event _) = True
        canTransition _ = False

-- A basic block in Smudge is the maximum homogeneous sequence of self events
-- and transient states, given an initially empty queue.  Self events by
-- themselves can only result in a state change to some proximate state.
-- Foreign calls can introduce arbitrary behavior, including branching and
-- non-determinism.  Events to other machines have no execution guarantees;
-- they may execute immediately, with some delay, or hypothetically even not at
-- all, and so are considered non-deterministic.
--
-- Sending events and calling functions are synchronous, but handling events is
-- asynchronous, so the events queued by foreign calls and other machines occur
-- after those sent explicitly.
--
-- Non-termination of a block is provable when a loop can go from some state to
-- the same state without a loop guard failing; the equivalent to a loop guard
-- in Smudge is equal initial conditions, viz., side effect queues.
--
-- Termination of a block is provable when the queue is empty, or the first
-- entry is not a self-event.
--
-- If none of these is true, termination is undecidable using this algorithm.
type BasicBlock = ([Event TaggedName], State TaggedName, [SideEffect TaggedName])

data StepState = StepState {
        path :: Seq (Event TaggedName),
        state :: State TaggedName,
        queue :: Seq (SideEffect TaggedName),
        queue' :: Seq (SideEffect TaggedName),
        visited :: Map (State TaggedName, Event TaggedName) (State TaggedName, Seq (SideEffect TaggedName))
    }

basicBlocks :: Graph gr => gr EnterExitState Happening -> [((State TaggedName, Event TaggedName), BasicBlock)]
basicBlocks g = foldMapWithKey (\event -> foldMapWithKey (\state _ -> case (state, event) of
                    (State _, Event _) -> [((state, event), build state event)]
                    otherwise          -> [])) ahs
    where ahs = allHandlers g
          onlyState s = filter ((s ==) . st . snd) $ labNodes g
          clone a = (a, a)

          build :: State TaggedName -> Event TaggedName -> BasicBlock
          build s e = (toList $ path final, state final, toList $ queue final >< queue' final)
                where final = fst $ until (Seq.null . queue . snd) (transients . step) $ transients (initial, initial)
                      initial = StepState {
                            queue = mempty |> (undefined, FuncEvent (undefined, e)),
                            path = mempty, state = s, queue' = mempty, visited = mempty
                        }

          transients :: (StepState, StepState) -> (StepState, StepState)
          transients stepstate@(_, prev@(StepState path s q q' visited)) = case out g $ fst $ head $ onlyState s of
            [(_, n', Happening {event=EventEnter, sideEffects})] -> case Map.lookup (s, EventEnter) visited of
                Just (s', q'') -> transients $ (prev, prev {state = s', queue' = q' >< q''})
                Nothing -> transients $ clone $ prev {state = s', queue = q >< q' >< q'', queue' = mempty, visited = visited'}
                    where Just (EnterExitState {st=s', en}) = lab g n'
                          q'' = fromList $ sideEffects ++ en
                          visited' = insert (s, EventEnter) (s', q'') visited
            otherwise -> stepstate

          step :: (StepState, StepState) -> (StepState, StepState)
          step stepstate@(_, prev@(StepState path s q q' visited)) = case viewl q of
                EmptyL -> stepstate
                (_, FuncEvent (_, e)) :< rest -> case Map.lookup (s, e) visited of
                    Just (s', q'') -> (prev, prev {path = path |> e, state = s', queue = rest, queue' = q' >< q''})
                    Nothing -> case Map.lookup e ahs >>= (! s) of
                        Just (s_h, e_h) -> clone $ StepState {path = path |> e, state = s', queue = rest >< q' >< q'', queue' = mempty, visited = visited'}
                            where [(n_h, EnterExitState {ex})]              = onlyState s_h
                                  [(_, n', Happening {sideEffects, flags})] = filter ((e_h ==) . event . edgeLabel) $ out g n_h
                                  Just (EnterExitState {st=s', en}) = lab g n'
                                  q'' = fromList $ sideEffects ++ (if NoTransition `elem` flags then [] else ex ++ en)
                                  visited' = insert (s, e) (s', q'') visited
                        Nothing -> clone $ prev {queue = mempty, queue' = q >< q'}
                otherwise -> clone $ prev {queue = mempty, queue' = q >< q'}
