module Main where

import PackageInfo (packageInfo, author, synopsis)
import Backends.Backend (options, generate)
import Backends.GraphViz (GraphVizOption(..))
import Backends.CStatic (CStaticOption(..))
import Model (smToGraph)
import Parsers.Smudge (state_machine, smudgle)
import Semantics (make_passes)

import Control.Applicative ((<$>), (<*>))
import Distribution.Package (packageVersion, packageName, PackageName(..))
import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Version (showVersion)

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

data Options = SystemOption SystemOption | GraphVizOption GraphVizOption | CStaticOption CStaticOption
    deriving (Show, Eq)

all_opts :: [OptDescr Options]
all_opts = concat [subcommand "" SystemOption sysopts,
                   (subcommand <$> fst <*> return CStaticOption <*> snd) options,
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
            compilationUnit <-
                if fileName == "-"
                    then getContents
                    else readFile fileName
            case parse smudgle fileName compilationUnit of
                Left err -> print err
                Right sms -> m sms
            where
                m sms = do
                    let gs = zip (map fst sms) (map smToGraph sms)
                    case and $ map (make_passes . snd) gs of
                        False -> report_failure
                        _ -> make_output gs
                report_failure = do
                    putStrLn "Semantic check failed."
                    exitFailure
                make_output gs = do
                    let filt o =
                            case o of
                                GraphVizOption a -> True
                                otherwise -> False
                    let gvos = map (\ (GraphVizOption a) -> a) $ filter filt os
                    outputNames <- generate gvos gs fileName
                    mapM_ putStrLn $ do outputName <- outputNames
                                        ["Wrote file \"" ++ outputName ++ "\""]
                    let filt' o =
                            case o of
                                CStaticOption a -> True
                                otherwise -> False
                    let csos = map (\ (CStaticOption a) -> a) $ filter filt' os
                    outputNames <- generate csos gs fileName
                    mapM_ putStrLn $ do outputName <- outputNames
                                        ["Wrote file \"" ++ outputName ++ "\""]

        (_,              _,  _) -> printUsage
