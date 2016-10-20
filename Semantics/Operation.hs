{-# LANGUAGE NamedFieldPuns #-}
module Semantics.Operation (
    handlers,
) where

import Grammars.Smudge (
  State(..),
  Event(..),
  )
import Model (
  EnterExitState(..),
  Happening(..),
  TaggedName,
  )

import Data.Graph.Inductive.Graph (
  labNodes,
  out,
  )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map (Map, fromList)

-- The `handlers` function enforces the precedence order for events:
-- 
-- state | event
-- =============
-- named | named
-- any   | named
-- named | any
-- any   | any

handlers :: Event TaggedName -> Gr EnterExitState Happening -> Map (State TaggedName) (Maybe (State TaggedName, Event TaggedName))
handlers e g = fromList $
    do  (_, EnterExitState {st=st@(State _)}) <- states
        case (filter ((== st) . fst) named_named, any_named, filter ((== st) . fst) named_any, any_any) of
            ([h], _, _, _) -> return (st, Just h)
            (_, [h], _, _) -> return (st, Just h)
            (_, _, [h], _) -> return (st, Just h)
            (_, _, _, [h]) -> return (st, Just h)
            (_, _, _, _  ) -> return (st, Nothing)
    where
        states = labNodes g
        all_handlers e = [(st ees, e) | (n, ees) <- states, (_, _, h) <- out g n, event h == e]

        all_evt_handlers = all_handlers e
        all_any_handlers = all_handlers EventAny
        named_named = [h | h@(State _, _) <- all_evt_handlers]
        any_named = [h | h@(StateAny, _) <- all_evt_handlers]
        named_any = [h | h@(State _, _) <- all_any_handlers]
        any_any = [h | h@(StateAny, _) <- all_any_handlers]
