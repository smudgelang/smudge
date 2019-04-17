-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Parser.Smudge (
    smudge_file,
    smudgle,
) where

import Language.Smudge.Grammar (
    StateMachine(StateMachine, StateMachineSame),
    State(State, StateSame),
    Event(Event, EventEnter),
    QEvent,
    Function(FuncVoid, FuncEvent),
    SideEffect,
    EventHandler,
    StateFlag(Initial),
    WholeState,
    )
import Language.Smudge.Lexer.Token (
    TokenCat(..),
    Token(text),
    tok,
    )
import Language.Smudge.Parsers.Id (
    Identifier,
    identify,
    )

import Control.Arrow ((&&&))
import Data.Semigroup ((<>))
import Text.Parsec (
    ParsecT,
    Stream,
    many,
    many1,
    sepEndBy,
    sepEndBy1,
    option,
    try,
    (<|>),
    (<?>),
    )

-- Smudge parsing
smudge_file :: Stream s m Token => ParsecT s u m ([[String]], [(StateMachine Identifier, [WholeState Identifier])])
smudge_file = (,) <$> many pragma <*> smudgle

-- Pragmas
pragma :: Stream s m Token => ParsecT s u m [String]
pragma = (:) <$> (tok PRAGMA *> pure "--") <> (text <$> tok COMMAND)
             <*> option [] (pure <$> text <$> tok ARG)

-- Module
smudgle :: Stream s m Token => ParsecT s u m [(StateMachine Identifier, [WholeState Identifier])]
smudgle = many1 state_machine_def

-- Machine
state_machine_def :: Stream s m Token => ParsecT s u m (StateMachine Identifier, [WholeState Identifier])
state_machine_def = (,) <$> state_machine_name <*> state_machine

state_machine_name :: Stream s m Token => ParsecT s u m (StateMachine Identifier)
state_machine_name = StateMachine <$> identify <$> tok ID

state_machine :: Stream s m Token => ParsecT s u m [WholeState Identifier]
state_machine = tok SMBEGIN *> state_machine_body <* tok SMEND

state_machine_body :: Stream s m Token => ParsecT s u m [WholeState Identifier]
state_machine_body = state_def `sepEndBy1` tok COMMA

-- State

state_def :: Stream s m Token => ParsecT s u m (WholeState Identifier)
state_def = do
    start <- option [] (tok START *> pure [Initial])
    name  <- state_name
    (en, ehs, ex) <- (arrow >>= \(ses, s) -> return ([], [(EventEnter, ses, s)], []))
                     <|> (,,) <$> enter_exit <*> state <*> enter_exit
    return (name, start, en, ehs, ex)

state_name :: Stream s m Token => ParsecT s u m (State Identifier)
state_name = State <$> identify <$> tok ID

enter_exit :: Stream s m Token => ParsecT s u m [SideEffect Identifier]
enter_exit = option [] $ tok EEBEGIN *> side_effect_list <* tok EEEND

state :: Stream s m Token => ParsecT s u m [EventHandler Identifier]
state = tok STBEGIN *> state_body <* tok STEND

state_body :: Stream s m Token => ParsecT s u m [EventHandler Identifier]
state_body = event_handler `sepEndBy1` tok COMMA

-- Handler

event_handler :: Stream s m Token => ParsecT s u m (EventHandler Identifier)
event_handler =
    event_name >>= \ev -> (,,) ev <$> dash <*> pure StateSame <|> uncurry ((,,) ev) <$> arrow
                          <?> "state transition for event \"" ++ show ev ++ "\""

event_name :: Stream s m Token => ParsecT s u m (Event Identifier)
event_name = Event <$> identify <$> tok ID

-- Transitions

dash :: Stream s m Token => ParsecT s u m [SideEffect Identifier]
dash  = try $ tok DASH  *> pure [] <|> tok DBEGIN *> side_effect_list <* tok DEND

arrow :: Stream s m Token => ParsecT s u m ([SideEffect Identifier], State Identifier)
arrow = try $ (,) <$> (tok ARROW *> pure [] <|> tok DBEGIN *> side_effect_list <* tok AEND) <*> state_name

-- Side effects

side_effect_list :: Stream s m Token => ParsecT s u m [SideEffect Identifier]
side_effect_list = side_effect `sepEndBy` tok COMMA

side_effect :: Stream s m Token => ParsecT s u m (SideEffect Identifier)
side_effect = (,) <$> function_call <*> pure FuncVoid
          <|> ((\(Event e) -> e) . snd &&& FuncEvent) <$> qualified_event
          <?> "side effect"

function_call :: Stream s m Token => ParsecT s u m Identifier
function_call = identify <$> tok FID

qualified_event :: Stream s m Token => ParsecT s u m (QEvent Identifier)
qualified_event = (,) <$> (try (state_machine_name <* tok DOT) <|> pure StateMachineSame)
                      <*> event_name
