module Parsers.Id (
    c_identifier,
    identifier,
    unquoted,
) where

import Grammars.Smudge (Name)

import Text.ParserCombinators.Parsec (
  Parser,
  many,
  many1,
  oneOf,
  try,
  (<|>),
  char,
  letter,
  digit,
  space,
  alphaNum,
  )

c_identifier :: Parser Name
c_identifier = (:) <$> nondigit <*> many (nondigit <|> digit)

identifier :: Parser Name
identifier = unquoted <|> quoted

unquoted :: Parser Name
unquoted = try ((:) <$> id_char <*> (many1 id_char))
           <|> many1 (alphaNum <|> (char '-'))

quoted :: Parser Name
quoted = ((char '"') *> many1 (id_char <|> space <|> symbol) <* (char '"'))

id_char :: Parser Char
id_char = (alphaNum <|> sep)

sep :: Parser Char
sep = oneOf "-_"

nondigit :: Parser Char
nondigit = letter <|> char '_'

symbol :: Parser Char
symbol = oneOf "!#$%&'()*+,./{|}~[\\]^`<>;="
