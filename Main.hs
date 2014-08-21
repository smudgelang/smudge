module Main where

import Paths_smudge
import Backends.Backend
import Backends.GraphViz
import Grammar

import Control.Applicative
import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Console.GetOpt
import System.Environment
import System.FilePath
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Version (showVersion)
import qualified Data.Map as Map

smToGraph :: (StateMachine, [(State, [(Event, [SideEffect], State)])]) -> Gr State EventAndSideEffects
smToGraph (sm, ss) = mkGraph [s | s <- zip [1..] (map fst ss)] es
                     where sn = Map.fromList [s | s <- zip (map fst ss) [1..]]
                           mkEdge s s'' e ses = (sn Map.! s, sn Map.! s'', EventAndSideEffects e ses)
                           es = [ese | ese <- concat $ map (\ (s, es) -> 
                                                            map (\ (e, ses, s') ->
                                                                 let s'' = case s' of
                                                                                StateSame -> s
                                                                                otherwise -> s'
                                                                 in mkEdge s s'' e ses) es) ss]

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

header :: String
header = "Usage: [OPTIONS] file"

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
printVersion = putStrLn ("Version: " ++ showVersion version)

main = do
    args <- getArgs
    case getOpt Permute all_opts args of
        (os,             _, [])  | elem (SystemOption Help) os -> printUsage
                                 | elem (SystemOption Version) os -> printVersion
        (os, (fileName:as),  _) -> do compilationUnit <- readFile fileName
                                      case parse state_machine fileName compilationUnit of
                                          Left err -> print err
                                          Right sm -> do let g = smToGraph sm
                                                         let gvos = map (\ (GraphVizOption a) -> a) $
                                                                        filter (\ o -> case o of
                                                                                 GraphVizOption a -> True
                                                                                 otherwise -> False) os
                                                         generate gvos g fileName
                                                         print $ labNodes g
                                                         print $ labEdges g
        (_,              _,  _) -> printUsage
