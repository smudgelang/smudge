module Grammar where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), empty)

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

event_handler_list :: Parser [(String, Maybe String)]
event_handler_list = sepBy (empty *> event_handler) (empty >> char ',')

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
