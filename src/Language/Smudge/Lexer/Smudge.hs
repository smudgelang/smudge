-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Lexer.Smudge (
    smudge_file,
) where

import Language.Smudge.Lexer.Char (
    newline,
    space,
    whitespace,
    idchar,
    visible,
    )
import Language.Smudge.Lexer.Id (
    identifier,
    foreign_identifier,
    )
import Language.Smudge.Lexer.Token (
    TokenCat(..),
    TokenStream,
    streamToken,
    tokenize,
    )

import Data.Semigroup ((<>))
import Text.Parsec (
    ParsecT,
    Stream,
    many,
    many1,
    option,
    try,
    (<|>),
    )
import Text.Parsec.Char (string)

-- Tokenizing
strtok :: Stream s m Char => TokenCat -> String -> ParsecT s u m TokenStream
strtok c = tokenize c . try . string

optional :: (Stream s m t, Monoid a) => ParsecT s u m a -> ParsecT s u m a
optional = option mempty

-- Whitespace
tok_newline :: Stream s m Char => ParsecT s u m TokenStream
tok_newline = tokenize NL newline

tok_whitespaces :: Stream s m Char => ParsecT s u m TokenStream
tok_whitespaces = optional (tokenize WS $ many1 whitespace)

tok_spaces :: Stream s m Char => ParsecT s u m TokenStream
tok_spaces = optional (tokenize WS $ many1 space)

tok_spaces1 :: Stream s m Char => ParsecT s u m TokenStream
tok_spaces1 = tokenize WS (many1 space)

-- Comments
comment :: Stream s m Char => ParsecT s u m TokenStream
comment = tokenize COMMENT (string "//" <> many (space <|> visible)) <> tok_newline

empty :: Stream s m Char => ParsecT s u m TokenStream
empty = tok_whitespaces <> optional (comment <> empty)

emptytoeol :: Stream s m Char => ParsecT s u m TokenStream
emptytoeol = optional $ tok_spaces <> (comment <|> tok_newline) <> emptytoeol

-- Smudge pragmas
pragma :: Stream s m Char => ParsecT s u m TokenStream
pragma = strtok PRAGMA "#" <> command <> optional (try $ tok_spaces1 <> argument) <> tok_spaces <> tok_newline

command :: Stream s m Char => ParsecT s u m TokenStream
command = tokenize COMMAND $ many1 idchar

argument :: Stream s m Char => ParsecT s u m TokenStream
argument = tokenize ARG $ many1 visible <> (mconcat <$> many (try $ many1 space <> many1 visible))

-- Smudge symbols
start_sigil :: Stream s m Char => ParsecT s u m TokenStream
start_sigil = strtok START "*"

comma :: Stream s m Char => ParsecT s u m TokenStream
comma = strtok COMMA ","

dot :: Stream s m Char => ParsecT s u m TokenStream
dot = strtok DOT "."

state_machine_begin :: Stream s m Char => ParsecT s u m TokenStream
state_machine_begin = strtok SMBEGIN "{"

state_machine_end :: Stream s m Char => ParsecT s u m TokenStream
state_machine_end   = strtok SMEND "}"

state_begin :: Stream s m Char => ParsecT s u m TokenStream
state_begin = strtok STBEGIN "["

state_end :: Stream s m Char => ParsecT s u m TokenStream
state_end   = strtok STEND "]"

dash_empty :: Stream s m Char => ParsecT s u m TokenStream
dash_empty = strtok DASH "--"

arrow_empty :: Stream s m Char => ParsecT s u m TokenStream
arrow_empty = strtok ARROW "-->"

dash_begin :: Stream s m Char => ParsecT s u m TokenStream
dash_begin = strtok DBEGIN "-("

dash_end :: Stream s m Char => ParsecT s u m TokenStream
dash_end = strtok DEND ")-"

arrow_end :: Stream s m Char => ParsecT s u m TokenStream
arrow_end = strtok AEND ")->"

enter_exit_begin :: Stream s m Char => ParsecT s u m TokenStream
enter_exit_begin = strtok EEBEGIN "("

enter_exit_end :: Stream s m Char => ParsecT s u m TokenStream
enter_exit_end   = strtok EEEND ")"

-- Smudge lexing
token :: Stream s m Char => ParsecT s u m TokenStream
token = start_sigil <|> comma <|> dot
    <|> state_machine_begin <|> state_machine_end <|> state_begin <|> state_end
    <|> arrow_empty <|> dash_empty <|> dash_begin <|> arrow_end <|> dash_end
    <|> enter_exit_begin <|> enter_exit_end
    <|> streamToken <$> identifier <|> streamToken <$> foreign_identifier 

smudge_file :: Stream s m Char => ParsecT s u m TokenStream
smudge_file = (mconcat <$> many (try (emptytoeol <> pragma))) <> empty <> (mconcat <$> many (token <> empty))
