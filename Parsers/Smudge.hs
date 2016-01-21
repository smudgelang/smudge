module Parsers.Smudge (
    state_machine,
    smudgle,
) where

import Grammars.Smudge (
  StateMachine(..),
  State(..),
  Event(..),
  SideEffect(..),
  EventHandler,
  StateFlag(..),
  WholeState
  )

import Text.ParserCombinators.Parsec hiding (State)
import Control.Applicative hiding ((<|>), empty, many)
import Data.Foldable (toList)

smudgle :: Parser [(StateMachine, [WholeState])]
smudgle = many1 state_machine

state_machine :: Parser (StateMachine, [WholeState])
state_machine = (,) <$> (empty *> state_machine_name <* empty) <*> state_machine_spec <* empty

state_machine_spec :: Parser [WholeState]
state_machine_spec = (char '{' >> empty) *> state_list <* (empty >> char '}')

state_list :: Parser [WholeState]
state_list = sepEndBy (state <* empty) (char ',' >> empty)

state :: Parser WholeState
state = try (uncurry (,,,,) <$> state_title <* empty <*> pure []
                            <*> ((\ (ses, s) -> [(EventEnter, ses, s)]) <$> to_state) <*> pure [])
         <|> uncurry (,,,,) <$> state_title <* empty <*> enter_exit_function <* empty
                            <*> event_handler_spec <* empty <*> enter_exit_function

event_handler_spec :: Parser [EventHandler]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [EventHandler]
event_handler_list = sepEndBy (event_handler <* empty) (char ',' >> empty)

enter_exit_function :: Parser [SideEffect]
enter_exit_function = option [] side_effect_container

side_effect_container :: Parser [SideEffect]
side_effect_container = (char '(' >> empty) *> side_effect_list <* (empty >> char ')')

to_state :: Parser ([SideEffect], State)
to_state = (,) <$> arrow <* empty <*> state_name

dash :: Parser [SideEffect]
dash = (char '-') *> option [] side_effect_container <* (char '-')

arrow :: Parser [SideEffect]
arrow = dash <* (char '>')

event_handler :: Parser EventHandler
event_handler =
    do ev <- event_name <|> event_any
       empty
       try ((\ (ses, s) -> (ev, ses, s)) <$> to_state)
        <|> (dash <* empty >>= \ses -> return (ev, ses, StateSame))
        <?> "state transition for event \"" ++ show ev ++ "\""

side_effect_list :: Parser [SideEffect]
side_effect_list = sepEndBy (side_effect <* empty) (char ',' >> empty)

side_effect :: Parser SideEffect
side_effect = try typed_function_call
              <|> try (FuncVoid <$> function_call)
              <|> (FuncDefault <$> qualified_event)
              <?> "side effect"

typed_function_call :: Parser SideEffect
typed_function_call = FuncEvent <$> function_call <* (empty >> char ':' >> empty) <*> qualified_event

qualified_event :: Parser (StateMachine, Event)
qualified_event = try ((,) <$> state_machine_name <* (char '.') <*> event_name)
                   <|> (,) <$> return StateMachineSame <*> event_name

function_call :: Parser String
function_call = char '@' *> c_identifier

state_title :: Parser (State, [StateFlag])
state_title = (,) <$> state_name <*> pure []
              <|> (,) <$> state_any <*> pure []
              <|> (,) <$> (char '*' >> empty *> state_name) <*> pure [Initial]

state_machine_name :: Parser StateMachine
state_machine_name = StateMachine <$> identifier

state_name :: Parser State
state_name = State <$> identifier

event_name :: Parser Event
event_name = Event <$> identifier

state_any :: Parser State
state_any = char '_' *> return StateAny

event_any :: Parser Event
event_any = char '_' *> return EventAny

comment :: Parser ()
comment = string "//" >> skipMany (noneOf "\r\n")

identifier :: Parser String
identifier = try ((:) <$> id_char <*> (many1 id_char))
             <|> many1 (alphaNum <|> (char '-'))
             <|> quoted

c_identifier :: Parser String
c_identifier = (:) <$> nondigit <*> many (nondigit <|> digit)

sep :: Parser Char
sep = oneOf "-_"

symbol :: Parser Char
symbol = oneOf "!#$%&'()*+,./{|}~[\\]^`<>;"

id_char :: Parser Char
id_char = (alphaNum <|> sep)

empty :: Parser ()
empty = try (spaces *> comment *> empty) <|> spaces

nondigit :: Parser Char
nondigit = letter <|> char '_'

quoted :: Parser String
quoted = ((char '"') *> many1 (id_char <|> space <|> symbol) <* (char '"'))
