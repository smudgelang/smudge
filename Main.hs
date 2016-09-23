module Main where

import PackageInfo (packageInfo, author, synopsis)
import Backends.Backend (options, generate)
import Backends.GraphViz (GraphVizOption(..))
import Backends.CStatic (CStaticOption(..))
import Model (passInitialState, passFullyQualify, passWholeStateToGraph, passGraphWithSymbols, passUniqueSymbols)
import Parsers.Smudge (state_machine, smudgle)
import Semantics.Semantic (Severity(..), Fault(..))
import Semantics (make_passes)
import Trashcan.Graph

import Control.Applicative ((<$>), (<*>))
import Distribution.Package (packageVersion, packageName, PackageName(..))
import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(..))
import System.Environment (getArgs)
import System.FilePath (joinPath)
import System.Exit (exitFailure)
import Data.Version (showVersion)
import Data.Monoid (mempty)

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

data SystemOption = Version | Help | OutDir FilePath
    deriving (Show, Eq)

app_name :: String
app_name = ((\ (PackageName s) -> s) $ packageName packageInfo)

header :: String
header = "Usage: " ++ app_name ++ " [OPTIONS] file\n" ++
         synopsis ++ "\n" ++
         "Written by " ++ author ++ "\n"

sysopts :: [OptDescr SystemOption]
sysopts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
           Option ['h'] ["help"] (NoArg Help) "Print this message.",
           Option []    ["outdir"] (ReqArg OutDir "DIR") "Output directory."]

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

--processFile :: String -> IO ([(StateMachine, Gr EnterExitState Happening)], SymbolTable)
processFile fileName = do
    compilationUnit <-
        if fileName == "-"
            then getContents
            else readFile fileName
    case parse smudgle fileName compilationUnit of
        Left err -> print_exit (show err)
        Right sms -> m sms
    where
        m sms = do
            let sms' = passInitialState sms
            let sms'' = passFullyQualify sms'
            let gs = passWholeStateToGraph sms''
            let fs = concat $ map make_passes gs
            mapM (putStrLn . show) fs
            case [f | f@(Fault ERROR _ _) <- fs] ++
                 [f | f@(Fault BUG _ _) <- fs] of
                [] -> do
                    let gswst = passGraphWithSymbols gs
                    case True of
                        False -> report_failure 0
                        _ -> return $ passUniqueSymbols gswst
                _  -> report_failure $ length fs
        report_failure n =
            print_exit ("Exiting with " ++ show n ++ " error" ++ (if n == 1 then "" else "s"))
        print_exit e = do
            putStrLn e
            exitFailure
            return mempty

--make_output :: String -> [Options] -> ([(StateMachine, Gr EnterExitState Happening)], SymbolTable) -> IO ()
make_output fileName os gswust = do
    let gvos = [a | GraphVizOption a <- os]
    outputNames <- generate gvos gswust fileName
    mapM_ putStrLn $ do outputName <- outputNames
                        ["Wrote file \"" ++ outputName ++ "\""]
    let csos = [a | CStaticOption a <- os]
    outputNames <- generate csos gswust fileName
    mapM_ putStrLn $ do outputName <- outputNames
                        ["Wrote file \"" ++ outputName ++ "\""]

main = do
    args <- getArgs
    case getOpt Permute all_opts args of
        (os,             _, [])
            | elem (SystemOption Help) os -> printUsage
            | elem (SystemOption Version) os -> printVersion

        (os, (fileName:as),  _) -> processFile fileName >>= make_output
                                   (joinPath [(prefix os), fileName]) os

        (_,              _,  _) -> printUsage

    where
      prefix [] = "."
      prefix ((SystemOption (OutDir p)):_) = p
      prefix (_:t) = prefix t
