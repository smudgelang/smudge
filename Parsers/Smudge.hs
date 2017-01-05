module Parsers.Smudge (
    state_machine,
    smudgle,
) where

import Grammars.Smudge (
  Annotated(..),
  StateMachine(..),
  StateMachineDeclarator(..),
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

import Text.ParserCombinators.Parsec (
  Parser,
  getPosition,
  sepEndBy,
  sepEndBy1,
  many1,
  option,
  noneOf,
  skipMany,
  try,
  (<|>),
  (<?>),
  char,
  string,
  space,
  spaces,
  )

smudgle :: Parser [(StateMachine Identifier, [WholeState Identifier])]
smudgle = many1 state_machine

state_machine :: Parser (StateMachine Identifier, [WholeState Identifier])
state_machine = (,) <$> (empty *> sm_name_plus_pos <* empty) <*> state_machine_spec <* empty

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
side_effect = try typed_function_call
              <|> try ((,) <$> function_call <*> pure FuncVoid)
              <|> ((\q@(s, Event e) -> (e, FuncEvent q)) <$> qualified_event)
              <?> "side effect"

typed_function_call :: Parser (SideEffect Identifier)
typed_function_call = (,) <$> function_call <* (empty >> char ':' >> empty) <*> (FuncTyped <$> qualified_event)

qualified_event :: Parser (QEvent Identifier)
qualified_event = try ((,) <$> state_machine_name <* (char '.') <*> event_name)
                   <|> (,) <$> return StateMachineSame <*> event_name

function_call :: Parser Identifier
function_call = host_identifier

state_title :: Parser (State Identifier, [StateFlag])
state_title = (,) <$> state_name <*> pure []
              <|> (,) <$> state_any <*> pure []
              <|> (,) <$> (char '*' >> empty *> state_name) <*> pure [Initial]

state_machine_name :: Parser (StateMachineDeclarator Identifier)
state_machine_name = StateMachineDeclarator <$> identifier

sm_name_plus_pos :: Parser (StateMachine Identifier)
sm_name_plus_pos = do pos <- getPosition
                      sm <- state_machine_name
                      return (Annotated pos sm)

state_name :: Parser (State Identifier)
state_name = State <$> identifier

event_name :: Parser (Event Identifier)
event_name = Event <$> identifier

state_any :: Parser (State Identifier)
state_any = char '_' *> return StateAny

event_any :: Parser (Event Identifier)
event_any = char '_' *> return EventAny

comment :: Parser ()
comment = string "//" >> skipMany (noneOf "\r\n")

empty :: Parser ()
empty = try (spaces *> comment *> empty) <|> spaces

spacesep :: Parser ()
spacesep = (space >> empty) <|> (comment >> empty)
