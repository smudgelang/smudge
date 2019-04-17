-- Copyright 2019 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

module Language.Smudge.Parsers.Id (
    Name,
    Location,
    showLineCol,
    Declared(..),
    Identifier,
    rawtest,
    mangle,
    identify,
) where

import Language.Smudge.Lexer.Token (TokenCat(ID, FID), Token(cat, pos, text))
import Language.Smudge.Lexer.Id (unquoted, identifier, foreign_identifier)

import Text.Parsec (
  parse,
  getInput,
  eof,
  (<|>),
  spaces,
  )

import Text.Parsec.Pos (
  SourcePos,
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
                          return (identify id, rest)

rawtest :: (Name -> Bool) -> Identifier -> Bool
rawtest f (Identifier _ (RawId name)) = f name
rawtest _ (Identifier _ (CookedId name)) = False

mangle :: (Name -> Name) -> Identifier -> Name
mangle f (Identifier _ (RawId name)) = f name
mangle _ (Identifier _ (CookedId name)) = name

identify :: Token -> Identifier
identify t = Identifier (pos t) (idCtor $ text t)
    where idCtor = case cat t of
            ID -> RawId; FID -> CookedId
            otherwise -> error "Expected identifier.  This is a bug in smudge."
