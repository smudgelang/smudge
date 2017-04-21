module Parsers.Id (
    Name,
    Location,
    Declared(..),
    Identifier,
    host_identifier,
    identifier,
    rawtest,
    mangle,
) where

import Text.ParserCombinators.Parsec (
  Parser,
  getPosition,
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

import Text.ParserCombinators.Parsec.Pos (
  SourcePos,
  initialPos,
  )

import Data.Either (rights)
import Control.Arrow (first)

type Name = String
type Location = SourcePos

class Declared a where
    at :: a -> Location

data PureIdentifier = RawId Name | CookedId Name
    deriving (Eq, Ord)

instance Show PureIdentifier where
    show (RawId name) = case parse (unquoted <* eof) "" name of
                          Right _ -> name
                          Left _ -> show name
    show (CookedId name) = '@' : name

data Identifier = Identifier Location PureIdentifier

instance Eq Identifier where
    (Identifier _ a) == (Identifier _ b) = a == b

instance Ord Identifier where
    compare (Identifier _ a) (Identifier _ b) = compare a b

instance Declared Identifier where
    at (Identifier pos _) = pos

instance Show Identifier where
    show (Identifier _ a) = show a

instance Read Identifier where
    readsPrec d = readParen False
                    (\r -> rights [parse ident "" r])
        where ident = do  id <- spaces *> (host_identifier <|> identifier)
                          rest <- getInput
                          return (id, rest)

rawtest :: (Name -> Bool) -> Identifier -> Bool
rawtest f (Identifier _ (RawId name)) = f name
rawtest _ (Identifier _ (CookedId name)) = False

mangle :: (Name -> Name) -> Identifier -> Name
mangle f (Identifier _ (RawId name)) = f name
mangle _ (Identifier _ (CookedId name)) = name

host_identifier :: Parser Identifier
host_identifier = char '@' *> c_identifier

c_identifier :: Parser Identifier
c_identifier = Identifier <$> getPosition <*> (CookedId <$> ((:) <$> nondigit <*> many (nondigit <|> digit)))

identifier :: Parser Identifier
identifier = Identifier <$> getPosition <*> (RawId <$> (unquoted <|> quoted))

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
