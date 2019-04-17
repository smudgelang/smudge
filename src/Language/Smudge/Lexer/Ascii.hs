-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Lexer.Ascii (
    ascii_visible,
    ascii_visible_nonquote,
    ascii_nl,
    ascii_space,
    ascii_alpha,
    ascii_digit,
) where

import Text.Parsec ((<|>))
import Text.Parsec (ParsecT, Stream)
import Text.Parsec.Char (char, oneOf)

-- Visible characters
-- The excluded characters are either below in ascii_nl or ascii_space, or are
-- control characters.
ascii_visible :: Stream s m Char => ParsecT s u m Char
ascii_visible = oneOf ['\x21' .. '\x7E']

ascii_visible_nonquote :: Stream s m Char => ParsecT s u m Char
ascii_visible_nonquote = oneOf $ '\x21' : ['\x23' .. '\x7E']

-- Whitespace
ascii_nl :: Stream s m Char => ParsecT s u m Char
ascii_nl = char '\x0D' *> char '\x0A' <|> oneOf ['\x0A' .. '\x0D']

ascii_space :: Stream s m Char => ParsecT s u m Char
ascii_space = oneOf "\x09\x20"

-- Letters
ascii_lower :: Stream s m Char => ParsecT s u m Char
ascii_lower = oneOf ['\x61' .. '\x7A']

ascii_upper :: Stream s m Char => ParsecT s u m Char
ascii_upper = oneOf ['\x41' .. '\x5A']

ascii_alpha :: Stream s m Char => ParsecT s u m Char
ascii_alpha = ascii_lower <|> ascii_upper

-- Numbers
ascii_digit :: Stream s m Char => ParsecT s u m Char
ascii_digit = oneOf ['\x30' .. '\x39']
