-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Lexer.Id (
    unquoted,
    identifier,
    foreign_identifier,
) where

import Language.Smudge.Lexer.Ascii (ascii_alpha, ascii_digit)
import Language.Smudge.Lexer.Char (whitespace, idchar, visible_nonquote)
import Language.Smudge.Lexer.Token (TokenCat(ID, FID), Token, token)

import Data.Semigroup ((<>))
import Text.Parsec (ParsecT, Stream, many, many1, between, (<|>))
import Text.Parsec.Char (char)

-- C identifiers
ascii_nondigit :: Stream s m Char => ParsecT s u m Char
ascii_nondigit = ascii_alpha <|> char '_'

c_identifier :: Stream s m Char => ParsecT s u m String
c_identifier = fmap pure ascii_nondigit <> many (ascii_nondigit <|> ascii_digit)

-- Smudge identifiers
unquoted :: Stream s m Char => ParsecT s u m String
unquoted = many1 idchar

quoted :: Stream s m Char => ParsecT s u m String
quoted = between (char '"') (char '"') $ many1 (whitespace <|> visible_nonquote)

identifier :: Stream s m Char => ParsecT s u m Token
identifier = token ID (unquoted <|> quoted)

foreign_identifier :: Stream s m Char => ParsecT s u m Token
foreign_identifier = token FID (char '@' >> c_identifier)
