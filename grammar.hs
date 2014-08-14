module Grammar where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), empty)

state_machine :: Parser (String, [(String, [(String, Maybe String)])])
state_machine =
    do sm <- state_machine_name
       empty
       sms <- state_machine_spec
       empty
       return (sm, sms)

state_machine_spec :: Parser [(String, [(String, Maybe String)])]
state_machine_spec = (char '{' >> empty) *> transition_specifier <* (empty >> char '}')

transition_specifier :: Parser [(String, [(String, Maybe String)])]
transition_specifier = state_list

state :: Parser (String, [(String, Maybe String)])
state = 
    do sn <- state_name 
       empty
       ehs <- event_handler_spec
       empty
       return (sn, ehs)

event_handler_spec :: Parser [(String, Maybe String)]
event_handler_spec = (char '[' >> empty) *> event_handler_list <* (empty >> char ']')

event_handler_list :: Parser [(String, Maybe String)]
event_handler_list = sepBy (event_handler <* empty) (char ',' >> empty)

state_list :: Parser [(String, [(String, Maybe String)])]
state_list = sepBy (state <* empty) (char ',' >> empty)

event_handler :: Parser (String, Maybe String)
event_handler =
    do ev <- event_name
       empty
       try (string "-->" >> empty >> state_name >>= \st -> return (ev, Just st))
        <|> (string "--" >> empty >> return (ev, Nothing))
        <?> "state transition for event \"" ++ ev ++ "\""

state_machine_name :: Parser String
state_machine_name = identifier

state_name :: Parser String
state_name = identifier

event_name :: Parser String
event_name = identifier

identifier :: Parser String
identifier = try ((:) <$> id_char <*> (many1 id_char))
             <|> many1 (alphaNum <|> (char '-'))
             <|> ((char '"') >> many1 (id_char <|> space <|> symbol) <* (char '"'))

sep :: Parser Char
sep = oneOf "-_"

symbol :: Parser Char
symbol = oneOf "!#$%&'()*+,./{|}~[\\]^`"

id_char :: Parser Char
id_char = (alphaNum <|> sep)

empty :: Parser ()
empty = spaces
