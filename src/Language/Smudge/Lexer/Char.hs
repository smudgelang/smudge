-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Lexer.Char (
    newline,
    space,
    whitespace,
    idchar,
    visible,
    visible_nonquote,
) where

import Text.Parsec (ParsecT, Stream, (<|>))
import Text.Parsec.Char (oneOf)

import Language.Smudge.Lexer.Ascii (
    ascii_visible,
    ascii_visible_nonquote,
    ascii_nl,
    ascii_space,
    ascii_alpha,
    ascii_digit,
    )
import Language.Smudge.Lexer.Unicode (
    unicode_visible,
    unicode_nl,
    unicode_space,
    unicode_alpha,
    unicode_digit,
    unicode_num,
    )

-- Whitespace
newline :: Stream s m Char => ParsecT s u m Char
newline = ascii_nl <|> unicode_nl

space :: Stream s m Char => ParsecT s u m Char
space = ascii_space <|> unicode_space

whitespace :: Stream s m Char => ParsecT s u m Char
whitespace = space <|> newline

-- Letters
alpha :: Stream s m Char => ParsecT s u m Char
alpha = ascii_alpha <|> unicode_alpha

-- Numbers
digit :: Stream s m Char => ParsecT s u m Char
digit = ascii_digit <|> unicode_digit

number :: Stream s m Char => ParsecT s u m Char
number = digit <|> unicode_num

-- Smudge identifiers
idchar :: Stream s m Char => ParsecT s u m Char
idchar = alpha <|> number <|> oneOf "-_"

-- Visible characters
visible :: Stream s m Char => ParsecT s u m Char
visible = ascii_visible <|> unicode_visible

visible_nonquote :: Stream s m Char => ParsecT s u m Char
visible_nonquote = ascii_visible_nonquote <|> unicode_visible
