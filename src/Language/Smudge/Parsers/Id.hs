-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

module Language.Smudge.Parsers.Id (
    Name,
    Location,
    showLineCol,
    Declared(..),
    Identifier,
    foreign_identifier,
    identifier,
    rawtest,
    mangle,
) where

import Text.Parsec.String (
  Parser,
  )

import Text.Parsec (
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

import Text.Parsec.Pos (
  SourcePos,
  initialPos,
  setSourceName,
  )

import Data.Either (rights)
import Control.Arrow (first)

type Name = String
type Location = SourcePos

showLineCol :: Location -> String
showLineCol pos = show $ setSourceName pos ""

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
        where ident = do  id <- spaces *> (foreign_identifier <|> identifier)
                          rest <- getInput
                          return (id, rest)

rawtest :: (Name -> Bool) -> Identifier -> Bool
rawtest f (Identifier _ (RawId name)) = f name
rawtest _ (Identifier _ (CookedId name)) = False

mangle :: (Name -> Name) -> Identifier -> Name
mangle f (Identifier _ (RawId name)) = f name
mangle _ (Identifier _ (CookedId name)) = name

foreign_identifier :: Parser Identifier
foreign_identifier = char '@' *> c_identifier

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
