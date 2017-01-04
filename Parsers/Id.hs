module Parsers.Id (
    Name,
    Identifier,
    c_identifier,
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
  alphaNum,
  )

type Name = String

data Identifier = RawId Name | CookedId Name
    deriving (Eq, Ord)

instance Show Identifier where
    show (RawId name) = case parse (unquoted <* eof) "" name of
                          Right _ -> name
                          Left _ -> show name
    show (CookedId name) = '@' : name

instance Read Identifier where
    readsPrec d = readParen (d > app_prec)
                    (\r -> either (const []) id $
                                parse ident "" r)
        where app_prec = 10
              ident = do  id <- c_identifier <|> identifier
                          rest <- getInput
                          return [(id, rest)]

mangle :: (Name -> Name) -> Identifier -> Name
mangle f (RawId name) = f name
mangle _ (CookedId name) = name

c_identifier :: Parser Identifier
c_identifier = CookedId <$> (char '@' *> ((:) <$> nondigit <*> many (nondigit <|> digit)))

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
