module Grammar where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), empty, many)

state_machine :: Parser (String, [(String, Maybe String, [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))], Maybe String)])
state_machine = (,) <$> (empty *> state_machine_name <* empty) <*> state_machine_spec <* empty

state_machine_spec :: Parser [(String, Maybe String, [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))], Maybe String)]
state_machine_spec = (char '{' >> empty) *> state_list <* (empty >> char '}')

--TODO
state_list :: Parser [(String, Maybe String, [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))], Maybe String)]
state_list = sepBy (state <* empty) (char ',' >> empty)

state :: Parser (String, Maybe String, [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))], Maybe String)
state = (,,,) <$> state_title <* empty <*> enter_function <* empty
              <*> event_handler_spec <* empty <*> exit_function

event_handler_spec :: Parser [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [(String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))]
event_handler_list = sepBy (event_handler <* empty) (char ',' >> empty)

enter_function :: Parser (Maybe String)
enter_function = optionMaybe function_call

exit_function :: Parser (Maybe String)
exit_function = optionMaybe function_call

side_effect_container :: Parser [(Maybe String, Maybe (Maybe String, String))]
side_effect_container = (char '(' >> empty) *> side_effect_list <* (empty >> char ')')

to_state :: Parser (Maybe [(Maybe String, Maybe (Maybe String, String))], String)
to_state = (,) <$> arrow <* empty <*> state_name

dash :: Parser (Maybe [(Maybe String, Maybe (Maybe String, String))])
dash = (char '-') *> optionMaybe side_effect_container <* (char '-')

arrow :: Parser (Maybe [(Maybe String, Maybe (Maybe String, String))])
arrow = dash <* (char '>')

--TODO
event_handler :: Parser (String, (Maybe [(Maybe String, Maybe (Maybe String, String))], Maybe String))
event_handler =
    do ev <- event_name
       empty
       try (to_state >>= \(ses, st) -> return (ev, (ses, Just st)))
        <|> (dash <* empty >>= \ses -> return (ev, (ses, Nothing)))
        <?> "state transition for event \"" ++ ev ++ "\""

side_effect_list :: Parser [(Maybe String, Maybe (Maybe String, String))]
side_effect_list = sepBy (side_effect <* empty) (char ',' >> empty)

side_effect :: Parser (Maybe String, Maybe (Maybe String, String))
side_effect = try (typed_function_call >>= \f -> return (Just (fst f), Just (snd f)))
              <|> try ((,) <$> (function_call >>= \f -> return (Just f)) <*> return Nothing)
              <|> (,) <$> return Nothing <*> (qualified_event >>= \e -> return (Just e))
              <?> "side effect"

typed_function_call :: Parser (String, (Maybe String, String))
typed_function_call = (,) <$> function_call <* (empty >> char ':' >> empty) <*> qualified_event

qualified_event :: Parser (Maybe String, String)
qualified_event = try ((,) <$> optionMaybe (state_machine_name <* (char '.')) <*> event_name)
                  <|> (,) <$> return Nothing <*> event_name

function_call :: Parser String
function_call = char '@' *> c_identifier

--TODO
state_title :: Parser String
state_title = state_name

state_machine_name :: Parser String
state_machine_name = identifier

state_name :: Parser String
state_name = identifier

event_name :: Parser String
event_name = identifier

--TODO
--any

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
