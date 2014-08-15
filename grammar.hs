module Grammar where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Applicative hiding ((<|>), empty, many)
import Data.Maybe (maybeToList)

data StateMachine = StateMachine String | StateMachineSame
    deriving (Show, Eq, Ord)

data State = State String | StateAny | StateSame
    deriving (Show, Eq, Ord)

data Event = Event String | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

state_machine :: Parser (StateMachine, [(State, [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))])])
state_machine = (,) <$> (empty *> state_machine_name <* empty) <*> state_machine_spec <* empty

state_machine_spec :: Parser [(State, [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))])]
state_machine_spec = (char '{' >> empty) *> state_list <* (empty >> char '}')

state_list :: Parser [(State, [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))])]
state_list = sepBy (state <* empty) (char ',' >> empty)

state :: Parser (State, [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))])
state = try ((,) <$> state_title <* empty
                 <*> ((:[]) <$> ((,) <$> return EventEnter <*> to_state)))
         <|> (,) <$> state_title <* empty <*> ((++) <$> ((++) <$> (maybeToList <$> enter_function <* empty)
                 <*> event_handler_spec <* empty) <*> (maybeToList <$> exit_function))

event_handler_spec :: Parser [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [(Event, ([(Maybe String, Maybe (StateMachine, Event))], State))]
event_handler_list = sepBy (event_handler <* empty) (char ',' >> empty)

enter_function :: Parser (Maybe (Event, ([(Maybe String, Maybe (StateMachine, Event))], State)))
enter_function = optionMaybe (function_call >>= \f -> return (EventEnter, ([(Just f, Nothing)], StateSame)))

exit_function :: Parser (Maybe (Event, ([(Maybe String, Maybe (StateMachine, Event))], State)))
exit_function = optionMaybe (function_call >>= \f -> return (EventExit, ([(Just f, Nothing)], StateSame)))

side_effect_container :: Parser [(Maybe String, Maybe (StateMachine, Event))]
side_effect_container = (char '(' >> empty) *> side_effect_list <* (empty >> char ')')

to_state :: Parser ([(Maybe String, Maybe (StateMachine, Event))], State)
to_state = (,) <$> arrow <* empty <*> state_name

dash :: Parser [(Maybe String, Maybe (StateMachine, Event))]
dash = (char '-') *> option [] side_effect_container <* (char '-')

arrow :: Parser [(Maybe String, Maybe (StateMachine, Event))]
arrow = dash <* (char '>')

event_handler :: Parser (Event, ([(Maybe String, Maybe (StateMachine, Event))], State))
event_handler =
    do ev <- event_name <|> event_any
       empty
       try ((,) <$> return ev <*> to_state)
        <|> (dash <* empty >>= \ses -> return (ev, (ses, StateSame)))
        <?> "state transition for event \"" ++ show ev ++ "\""

side_effect_list :: Parser [(Maybe String, Maybe (StateMachine, Event))]
side_effect_list = sepBy (side_effect <* empty) (char ',' >> empty)

side_effect :: Parser (Maybe String, Maybe (StateMachine, Event))
side_effect = try (typed_function_call >>= \f -> return (Just (fst f), Just (snd f)))
              <|> try ((,) <$> Just <$> function_call <*> return Nothing)
              <|> (,) <$> return Nothing <*> Just <$> qualified_event
              <?> "side effect"

typed_function_call :: Parser (String, (StateMachine, Event))
typed_function_call = (,) <$> function_call <* (empty >> char ':' >> empty) <*> qualified_event

qualified_event :: Parser (StateMachine, Event)
qualified_event = try ((,) <$> state_machine_name <* (char '.') <*> event_name)
                   <|> (,) <$> return StateMachineSame <*> event_name

function_call :: Parser String
function_call = char '@' *> c_identifier

state_title :: Parser State
state_title = state_name <|> state_any

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

--TODO
--comment

identifier :: Parser String
identifier = try ((:) <$> id_char <*> (many1 id_char))
             <|> many1 (alphaNum <|> (char '-'))
             <|> quoted

c_identifier :: Parser String
c_identifier = (:) <$> nondigit <*> many (nondigit <|> digit)

sep :: Parser Char
sep = oneOf "-_"

symbol :: Parser Char
symbol = oneOf "!#$%&'()*+,./{|}~[\\]^`"

id_char :: Parser Char
id_char = (alphaNum <|> sep)

empty :: Parser ()
empty = spaces

nondigit :: Parser Char
nondigit = letter <|> char '_'

quoted :: Parser String
quoted = ((char '"') *> many1 (id_char <|> space <|> symbol) <* (char '"'))
