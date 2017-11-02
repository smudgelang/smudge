-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

module Parsers.Smudge (
    smudge_file,
    state_machine,
    smudgle,
) where

import Grammars.Smudge (
  StateMachine(..),
  State(..),
  Event(..),
  QEvent,
  Function(..),
  SideEffect(..),
  EventHandler,
  StateFlag(..),
  WholeState
  )
import Parsers.Id (
  Identifier,
  identifier,
  host_identifier,
  )

import Text.Parsec.String (
  Parser,
  )

import Text.Parsec (
  sepEndBy,
  sepEndBy1,
  many,
  many1,
  option,
  optional,
  oneOf,
  noneOf,
  notFollowedBy,
  skipMany,
  try,
  (<|>),
  (<?>),
  char,
  anyChar,
  alphaNum,
  string,
  space,
  spaces,
  endOfLine,
  )

import Control.Monad (void)

smudge_file :: Parser ([[String]], [(StateMachine Identifier, [WholeState Identifier])])
smudge_file = emptytoeol >> (,) <$> many (pragma <* emptytoeol) <*> smudgle

pragma :: Parser [String]
pragma = (:) <$> (("--" ++) <$> (char '#' *> command)) <*> option [] ((string "=" <|> many1 whitespace) *> ((:[]) <$> argument)) <* endOfLine

command :: Parser String
command = many1 (alphaNum <|> oneOf "-_")

argument :: Parser String
argument = many1 non_newline

smudgle :: Parser [(StateMachine Identifier, [WholeState Identifier])]
smudgle = empty >> many1 state_machine

state_machine :: Parser (StateMachine Identifier, [WholeState Identifier])
state_machine = (,) <$> (state_machine_name <* empty) <*> state_machine_spec <* empty

state_machine_spec :: Parser [WholeState Identifier]
state_machine_spec = (char '{' >> empty) *> state_list <* (empty >> char '}')

state_list :: Parser [WholeState Identifier]
state_list = sepEndBy (state <* empty) (char ',' >> empty)

state :: Parser (WholeState Identifier)
state = try (uncurry (,,,,) <$> state_title <* spacesep <*> pure []
                            <*> ((\ (ses, s) -> [(EventEnter, ses, s)]) <$> to_state) <*> pure [])
         <|> uncurry (,,,,) <$> state_title <* empty <*> enter_exit_function <* empty
                            <*> event_handler_spec <* empty <*> enter_exit_function

event_handler_spec :: Parser [EventHandler Identifier]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [EventHandler Identifier]
event_handler_list = sepEndBy1 (event_handler <* empty) (char ',' >> empty)

enter_exit_function :: Parser [SideEffect Identifier]
enter_exit_function = option [] side_effect_container

side_effect_container :: Parser [SideEffect Identifier]
side_effect_container = (char '(' >> empty) *> side_effect_list <* (empty >> char ')')

to_state :: Parser ([SideEffect Identifier], State Identifier)
to_state = (,) <$> arrow <* empty <*> state_name

dash :: Parser [SideEffect Identifier]
dash = (char '-') *> option [] side_effect_container <* (char '-')

arrow :: Parser [SideEffect Identifier]
arrow = dash <* (char '>')

event_handler :: Parser (EventHandler Identifier)
event_handler =
    do ev <- event_name <|> event_any
       spacesep
       try ((\ (ses, s) -> (ev, ses, s)) <$> to_state)
        <|> (dash <* empty >>= \ses -> return (ev, ses, StateSame))
        <?> "state transition for event \"" ++ show ev ++ "\""

side_effect_list :: Parser [SideEffect Identifier]
side_effect_list = sepEndBy (side_effect <* empty) (char ',' >> empty)

side_effect :: Parser (SideEffect Identifier)
side_effect = try ((,) <$> function_call <*> pure FuncVoid)
              <|> ((\q@(s, Event e) -> (e, FuncEvent q)) <$> qualified_event)
              <?> "side effect"

qualified_event :: Parser (QEvent Identifier)
qualified_event = try ((,) <$> state_machine_name <* (char '.') <*> event_name)
                   <|> (,) <$> return StateMachineSame <*> event_name

function_call :: Parser Identifier
function_call = host_identifier

state_title :: Parser (State Identifier, [StateFlag])
state_title = (,) <$> state_name <*> pure []
              <|> (,) <$> state_any <*> pure []
              <|> (,) <$> (char '*' >> empty *> state_name) <*> pure [Initial]

state_machine_name :: Parser (StateMachine Identifier)
state_machine_name = StateMachine <$> identifier

state_name :: Parser (State Identifier)
state_name = State <$> identifier

event_name :: Parser (Event Identifier)
event_name = Event <$> identifier

state_any :: Parser (State Identifier)
state_any = char '_' *> return StateAny

event_any :: Parser (Event Identifier)
event_any = char '_' *> return EventAny

comment :: Parser ()
comment = string "//" >> skipMany non_newline >> void endOfLine

empty :: Parser ()
empty = spaces >> optional (comment >> empty)

emptytoeol :: Parser ()
emptytoeol = optional $ skipMany whitespace >> (comment <|> void endOfLine) >> emptytoeol

spacesep :: Parser ()
spacesep = (space >> empty) <|> (comment >> empty)

whitespace :: Parser Char
whitespace = exclude_newline space <?> "any non-new-line whitespace"

non_newline :: Parser Char
non_newline = exclude_newline anyChar <?> "any non-new-line character"

exclude_newline :: Parser Char -> Parser Char
exclude_newline p = notFollowedBy endOfLine *> p
