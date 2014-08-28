module Main where

import PackageInfo (packageInfo, author, synopsis)
import Backends.Backend (options, generate)
import Backends.GraphViz (GraphVizOption(..))
import Grammars.Smudge (StateMachine, State(..), Event, SideEffect, Happening(..))
import Parsers.Smudge (state_machine, smudgle)

import Control.Applicative ((<$>), (<*>))
import Distribution.Package (packageVersion, packageName, PackageName(..))
import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Environment (getArgs)
import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Version (showVersion)
import qualified Data.Map as Map

smToGraph :: (StateMachine, [(State, [(Event, [SideEffect], State)])]) ->
                 Gr State Happening
smToGraph (sm, ss) =
    -- Graph.mkGraph :: [(Node, a)] -> [(Node, Node, b)] -> gr a b
    mkGraph [s | s <- zip [1..] (map fst ss)] es
    where
        sn :: Map.Map State Node
        sn = Map.fromList [s | s <- zip (map fst ss) [1..]]
        mkEdge :: State -> State -> Happening ->
                    (Node, Node, Happening)
        mkEdge s s'' eses = (sn Map.! s, sn Map.! s'', eses)
        es = [ese | ese <- concat $ map f ss]
            where
                f :: (State, [(Event, [SideEffect], State)]) ->
                         [(Node, Node, Happening)]
                f (s, es) = map g es
                    where
                        g :: (Event, [SideEffect], State) ->
                                 (Node, Node, Happening)
                        g (e, ses, s') =
                            let (e', s'') = case s' of
                                    StateSame -> (Bustle e ses, s)
                                    otherwise -> (Hustle e ses, s')
                            in mkEdge s s'' e'

subcommand :: String -> (a -> b) -> [OptDescr a] -> [OptDescr b]
subcommand name f os = map makeSub os
    where makeSub (Option ls ws v d) = Option ls sws sv d
           where sws = case name of
                       "" -> ws
                       _  -> map ((name ++) . ('-' :)) ws
                 sv = case v of
                      NoArg a -> NoArg (f a)
                      ReqArg g s -> ReqArg (f . g) s
                      OptArg g s -> OptArg (f . g) s

data SystemOption = Version | Help
    deriving (Show, Eq)

app_name :: String
app_name = ((\ (PackageName s) -> s) $ packageName packageInfo)

header :: String
header = "Usage: " ++ app_name ++ " [OPTIONS] file\n" ++
         synopsis ++ "\n" ++
         "Written by " ++ author ++ "\n"

sysopts :: [OptDescr SystemOption]
sysopts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
           Option ['h'] ["help"] (NoArg Help) "Print this message."]

data Options = SystemOption SystemOption | GraphVizOption GraphVizOption
    deriving (Show, Eq)

all_opts :: [OptDescr Options]
all_opts = concat [subcommand "" SystemOption sysopts,
                   (subcommand <$> fst <*> return GraphVizOption <*> snd) options]

printUsage :: IO ()
printUsage = putStr $ usageInfo header all_opts

printVersion :: IO ()
printVersion = putStrLn (app_name ++ " version: " ++
                         (showVersion $ packageVersion packageInfo))

main = do
    args <- getArgs
    case getOpt Permute all_opts args of
        (os,             _, [])
            | elem (SystemOption Help) os -> printUsage
            | elem (SystemOption Version) os -> printVersion

        (os, (fileName:as),  _) -> do
            compilationUnit <- readFile fileName
            case parse smudgle fileName compilationUnit of
                Left err -> print err
                Right sms -> m sms
            where m sms = do
                    let gs = map smToGraph sms
                    let filt o =
                            case o of
                                GraphVizOption a -> True
                                otherwise -> False
                    let gvos = map (\ (GraphVizOption a) -> a) $ filter filt os
                    outputName <- generate gvos gs fileName
                    putStrLn $ "Wrote file \"" ++ outputName ++ "\""

        (_,              _,  _) -> printUsage

