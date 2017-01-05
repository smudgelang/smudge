module Parsers.Id (
    Name,
    Identifier,
    host_identifier,
    identifier,
    mangle,
) where

import Text.ParserCombinators.Parsec (
  Parser,
  parse,
  getInput,
  eof,
  many,
  many1,
  oneOf,
  try,
  (<|>),
  char,
  letter,
  digit,
  space,
  spaces,
  alphaNum,
  )

import Data.Either (rights)

type Name = String

data Identifier = RawId Name | CookedId Name
    deriving (Eq, Ord)

instance Show Identifier where
    show (RawId name) = case parse (unquoted <* eof) "" name of
                          Right _ -> name
                          Left _ -> show name
    show (CookedId name) = '@' : name

instance Read Identifier where
    readsPrec d = readParen False
                    (\r -> rights [parse ident "" r])
        where ident = do  id <- spaces *> (host_identifier <|> identifier)
                          rest <- getInput
                          return (id, rest)

mangle :: (Name -> Name) -> Identifier -> Name
mangle f (RawId name) = f name
mangle _ (CookedId name) = name

host_identifier :: Parser Identifier
host_identifier = char '@' *> c_identifier

c_identifier :: Parser Identifier
c_identifier = CookedId <$> ((:) <$> nondigit <*> many (nondigit <|> digit))

identifier :: Parser Identifier
identifier = RawId <$> (unquoted <|> quoted)

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
