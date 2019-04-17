-- Copyright 2019 Nate Bragg and Nathan Michaels.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/splat/blob/master/LICENSE

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Lexer.Token (
    TokenCat(..),
    Token(cat, pos, text),
    TokenStream,
    token,
    streamToken,
    tokenize,
    stripWhitespace,
    tok,
) where

import Data.Semigroup (Semigroup)
import Text.Parsec (
    ParsecT,
    Stream(uncons),
    mkPT,
    runParsecT,
    stateInput,
    tokenPrim,
    getPosition,
    (<?>),
    )
import Text.Parsec.Pos (SourcePos)

data TokenCat = COMMENT | WS | NL | PRAGMA | COMMAND | ARG | ID | FID
              | START | COMMA | DOT | SMBEGIN | SMEND | STBEGIN | STEND
              | DASH | ARROW | DBEGIN | DEND | AEND | EEBEGIN | EEEND
    deriving (Eq, Enum, Show)

catName :: TokenCat -> String
catName COMMENT = "comment"
catName WS      = "whitespace"
catName NL      = "newline"
catName PRAGMA  = "start of pragma"
catName COMMAND = "pragma command"
catName ARG     = "pragma argument"
catName ID      = "identifier"
catName FID     = "foreign identifier"
catName START   = "start sigil"
catName COMMA   = "comma"
catName DOT     = "dot"
catName SMBEGIN = "start of state machine definition"
catName SMEND   =   "end of state machine definition"
catName STBEGIN = "start of state definition"
catName STEND   =   "end of state definition"
catName DASH    = "dash"
catName ARROW   = "arrow"
catName DBEGIN  = "start of effect list"
catName DEND    =   "end of effect list" -- dash and arrow end are the same
catName AEND    =   "end of effect list" -- for the sake of error reporting
catName EEBEGIN = "start of transition effect list"
catName EEEND   =   "end of transition effect list"

data Token = Token {
    cat :: TokenCat,
    pos :: SourcePos,
    text :: String
}

instance Show Token where
    show t | cat t `elem` [ID, FID] = catName (cat t) ++ " " ++ show (text t)
    show t                          = catName (cat t)

newtype TokenStream = TokenStream { runStream :: [Token] }
    deriving (Semigroup, Monoid, Show)

instance Monad m => Stream TokenStream m Token where
    uncons = fmap (fmap (fmap TokenStream)) . uncons . runStream

class Tokenizable a where
    token :: Monad m => TokenCat -> ParsecT s u m a -> ParsecT s u m Token

instance Tokenizable Char where
    token c p = Token c <$> getPosition <*> fmap pure p <?> catName c

instance Tokenizable String where
    token c p = Token c <$> getPosition <*> p           <?> catName c

streamToken :: Token -> TokenStream
streamToken = TokenStream . (:[])

tokenize :: (Monad m, Tokenizable a) => TokenCat -> ParsecT s u m a -> ParsecT s u m TokenStream
tokenize c p = streamToken <$> token c p

stripWhitespace :: TokenStream -> TokenStream
stripWhitespace = TokenStream . filter (not . (`elem` [COMMENT, WS, NL]) . cat) . runStream

tok :: Stream s m Token => TokenCat -> ParsecT s u m Token
tok c = skip >>= \t' -> tokenPrim show
              (\_ t _ -> maybe (pos t) pos t')
              (\t -> if cat t == c then Just $ t else Nothing)
    <?> catName c
    where skip = mkPT $ \s -> do
            t <- maybe (pure Nothing) (fmap (fmap fst) . uncons . snd) =<< uncons (stateInput s)
            runParsecT (return t) s
