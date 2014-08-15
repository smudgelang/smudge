module Grammar where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Applicative hiding ((<|>), empty, many)
import Data.Maybe (maybeToList)

data State = State String | StateAny | StateSame
    deriving (Show, Eq, Ord)

data Event = Event String | EventAny | EventEnter | EventExit
    deriving (Show, Eq, Ord)

state_machine :: Parser (String, [(State, [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))])])
state_machine = (,) <$> (empty *> state_machine_name <* empty) <*> state_machine_spec <* empty

state_machine_spec :: Parser [(State, [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))])]
state_machine_spec = (char '{' >> empty) *> state_list <* (empty >> char '}')

state_list :: Parser [(State, [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))])]
state_list = sepBy (state <* empty) (char ',' >> empty)

state :: Parser (State, [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))])
state = try ((,) <$> state_title <* empty
                 <*> (to_state >>= \(ses, st) -> return [(EventEnter, (ses, Just st))]))
         <|> (,) <$> state_title <* empty <*> ((++) <$> ((++) <$> (maybeToList <$> enter_function <* empty)
                 <*> event_handler_spec <* empty) <*> (maybeToList <$> exit_function))

event_handler_spec :: Parser [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [(Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))]
event_handler_list = sepBy (event_handler <* empty) (char ',' >> empty)

enter_function :: Parser (Maybe (Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State)))
enter_function = optionMaybe (function_call >>= \f -> return (EventEnter, (Just [(Just f, Nothing)], Just StateSame)))

exit_function :: Parser (Maybe (Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State)))
exit_function = optionMaybe (function_call >>= \f -> return (EventExit, (Just [(Just f, Nothing)], Just StateSame)))

side_effect_container :: Parser [(Maybe String, Maybe (Maybe String, Event))]
side_effect_container = (char '(' >> empty) *> side_effect_list <* (empty >> char ')')

to_state :: Parser (Maybe [(Maybe String, Maybe (Maybe String, Event))], State)
to_state = (,) <$> arrow <* empty <*> state_name

dash :: Parser (Maybe [(Maybe String, Maybe (Maybe String, Event))])
dash = (char '-') *> optionMaybe side_effect_container <* (char '-')

arrow :: Parser (Maybe [(Maybe String, Maybe (Maybe String, Event))])
arrow = dash <* (char '>')

event_handler :: Parser (Event, (Maybe [(Maybe String, Maybe (Maybe String, Event))], Maybe State))
event_handler =
    do ev <- event_name <|> event_any
       empty
       try (to_state >>= \(ses, st) -> return (ev, (ses, Just st)))
        <|> (dash <* empty >>= \ses -> return (ev, (ses, Nothing)))
        <?> "state transition for event \"" ++ show ev ++ "\""

side_effect_list :: Parser [(Maybe String, Maybe (Maybe String, Event))]
side_effect_list = sepBy (side_effect <* empty) (char ',' >> empty)

side_effect :: Parser (Maybe String, Maybe (Maybe String, Event))
side_effect = try (typed_function_call >>= \f -> return (Just (fst f), Just (snd f)))
              <|> try ((,) <$> (function_call >>= \f -> return (Just f)) <*> return Nothing)
              <|> (,) <$> return Nothing <*> (qualified_event >>= \e -> return (Just e))
              <?> "side effect"

typed_function_call :: Parser (String, (Maybe String, Event))
typed_function_call = (,) <$> function_call <* (empty >> char ':' >> empty) <*> qualified_event

qualified_event :: Parser (Maybe String, Event)
qualified_event = try ((,) <$> optionMaybe (state_machine_name <* (char '.')) <*> event_name)
                  <|> (,) <$> return Nothing <*> event_name

function_call :: Parser String
function_call = char '@' *> c_identifier

state_title :: Parser State
state_title = state_name <|> state_any

state_machine_name :: Parser String
state_machine_name = identifier

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
