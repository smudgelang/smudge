-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE ExistentialQuantification #-}

module System.Console.GetOpt.Extra (
    ArgOrder(..),
    ArgDescr(..),
    OptDescr(..),
    usageInfo,
    getOpt
) where

import Data.List (partition, isPrefixOf, stripPrefix)
import System.Console.GetOpt (ArgOrder(..))
import qualified System.Console.GetOpt as GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..))

data ArgDescr a = BoolArg (Bool -> a)
                | NoArg a
                | ReqArg (String -> a) String
                | OptArg (Maybe String -> a) String
                | BoolOptArg (Bool -> Maybe String -> a) String

data OptDescr a = forall b . Subcommand String (b -> a) [OptDescr b]
                | Option [Char] [String] (ArgDescr a) String

subcommand :: String -> (a -> b) -> [GetOpt.OptDescr a] -> [GetOpt.OptDescr b]
subcommand name f os = map makeSub os
    where makeSub (GetOpt.Option ls ws v d) = GetOpt.Option ls sws (fmap f v) d
           where sws = case name of
                       "" -> ws
                       _  -> map ((name ++) . ('-' :)) ws

convertArgDescr :: Bool -> ArgDescr a -> GetOpt.ArgDescr a
convertArgDescr b (BoolArg p)  = GetOpt.NoArg (p b)
convertArgDescr _ (NoArg a)    = GetOpt.NoArg a
convertArgDescr _ (ReqArg c n) = GetOpt.ReqArg c n
convertArgDescr _ (OptArg c n) = GetOpt.OptArg c n
convertArgDescr b (BoolOptArg c n) = GetOpt.OptArg (c b) n

-- The option conversion is as follows:
--   if there are no negative long opts, the char opts are positive
--   if all long opts are negative, the char opts are negative
--   else, the char opts are positive, and the long opts are split
convertOptDescrs :: [OptDescr a] -> [GetOpt.OptDescr a]
convertOptDescrs = concatMap convertOptDescr
    where convertOptDescr :: OptDescr a -> [GetOpt.OptDescr a]
          convertOptDescr (Subcommand p c os) = subcommand p c (concatMap convertOptDescr os)
          convertOptDescr (Option ss ls a d) =
            case partition (isPrefixOf negate) (concatMap splitNeg ls) of
                ([], ps) -> [GetOpt.Option ss ps (convertArgDescr True a) d]
                (ns, []) -> [GetOpt.Option ss ns (convertArgDescr False a) d]
                (ns, ps) -> [GetOpt.Option ss ps (convertArgDescr True a) d,
                             GetOpt.Option [] ns (convertArgDescr False a) d]
          splitNeg lo = maybe [lo] (\lo -> [lo, negate ++ lo]) $ stripPrefix ("[" ++ negate ++ "]") lo
          negate = "no-"

formatOptDescrs :: [OptDescr a] -> [GetOpt.OptDescr a]
formatOptDescrs = concatMap formatOptDescr
    where formatOptDescr :: OptDescr a -> [GetOpt.OptDescr a]
          formatOptDescr (Subcommand p c os) = subcommand p c (concatMap formatOptDescr os)
          formatOptDescr (Option ss ls a d) = [GetOpt.Option ss ls (convertArgDescr False a) d]

usageInfo :: String -> [OptDescr a] -> String
usageInfo hdr = GetOpt.usageInfo hdr . formatOptDescrs

getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt o = GetOpt.getOpt o . convertOptDescrs
