module Grammar where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), empty)

state_machine :: Parser (String, [(String, [(String, Maybe String)])])
state_machine =
    do sm <- state_machine_name
       empty
       char '{'
       empty
       sl <- state_list
       empty
       char '}'
       empty
       return (sm, sl)

sep :: Parser Char
sep = oneOf "-_"

symbol :: Parser Char
symbol = oneOf "!#$%&'()*+,./{|}~[\\]^`"

id_char :: Parser Char
id_char = (alphaNum <|> sep)

empty :: Parser ()
empty = spaces

identifier :: Parser String
identifier = many1 (alphaNum <|> (char '-'))
             <|> (:) <$> id_char <*> (many1 id_char)
             <|> ((char '"') >> many1 (id_char <|> space <|> symbol) <* (char '"'))

state_machine_name :: Parser String
state_machine_name = identifier

state :: Parser (String, [(String, Maybe String)])
state = 
    do sn <- state_name 
       empty
       char '['
       empty
       el <- event_handler_list
       empty
       char ']'
       empty
       return (sn, el)

state_list :: Parser [(String, [(String, Maybe String)])]
state_list = sepBy (state <* empty) (char ',' >> empty)

event_handler_list :: Parser [(String, Maybe String)]
event_handler_list = sepBy (event_handler <* empty) (char ',' >> empty)

event_handler :: Parser (String, Maybe String)
event_handler =
    do ev <- event_name
       empty
       try (string "-->" >> empty >> state_name >>= \st -> return (ev, Just st))
        <|> (string "--" >> empty >> return (ev, Nothing))
        <?> "state transition for event \"" ++ ev ++ "\""

state_name :: Parser String
state_name = identifier

event_name :: Parser String
event_name = identifier
