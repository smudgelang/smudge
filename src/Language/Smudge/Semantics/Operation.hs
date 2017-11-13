-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE NamedFieldPuns #-}
module Language.Smudge.Semantics.Operation (
    handlers,
    finalStates,
) where

import Language.Smudge.Grammar (
  State(..),
  Event(..),
  )
import Language.Smudge.Semantics.Model (
  EnterExitState(..),
  Happening(..),
  HappeningFlag(..),
  TaggedName,
  )

import Data.Graph.Inductive.Graph (
  Node,
  labNodes,
  labEdges,
  nodes,
  out,
  )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (nub, (\\))
import Data.Map (Map, fromList, toList, (!))
import qualified Data.Map as Map(map)
import Control.Monad (guard)

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

handlers :: Event TaggedName -> Gr EnterExitState Happening -> Map (State TaggedName) (Maybe (State TaggedName, Event TaggedName))
handlers e g = allHandlers g ! e

allHandlers :: Gr EnterExitState Happening -> Map (Event TaggedName) (Map (State TaggedName) (Maybe (State TaggedName, Event TaggedName)))
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

finalStates :: Gr EnterExitState Happening -> [Node]
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
