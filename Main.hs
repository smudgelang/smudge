module Main where

import PackageInfo (packageInfo, author, synopsis)
import Backends.Backend (options, generate)
import Backends.GraphViz (GraphVizOption(..))
import Backends.CStatic (CStaticOption(..))
import Model (
  passInitialState,
  passFullyQualify,
  passRename,
  passTagCategories,
  passWholeStateToGraph,
  QualifiedName,
  )
import Semantics.Solver (
  elaborateMono,
  elaboratePoly,
  )
import Parsers.Smudge (smudge_file)
import Semantics.Alias (merge)
import Semantics.Basis (basisAlias, bindBasis)
import Semantics.Semantic (Severity(..), Fault(..), fatal)
import Semantics (make_passes, name_passes, type_passes)
import Trashcan.Graph

import Control.Monad (when)
import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Distribution.Package (packageVersion, packageName, PackageName(..))
import Text.Parsec (parse, eof)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(Permute))
import System.Environment (getArgs)
import System.FilePath (joinPath, takeFileName, dropFileName, normalise)
import System.Exit (exitFailure)
import Data.Either (lefts, rights, isLeft)
import Data.Map (fromList)
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

data SystemOption = Version
                  | Help
                  | OutDir FilePath
    deriving (Show, Eq)

data EnvmntOption = Strict
                  | Namespace String
                  | Rename String
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

envopts :: [OptDescr EnvmntOption]
envopts = [Option []    ["strict"] (NoArg Strict) "Require all types to match strictly",
           Option []    ["namespace"] (ReqArg Namespace "NEW") "Replace namespace.",
           Option []    ["rename"] (ReqArg Rename "\"OLD NEW\"") "Replace identifier."]

data Options = SystemOption SystemOption | EnvmntOption EnvmntOption | GraphVizOption GraphVizOption | CStaticOption CStaticOption
    deriving (Show, Eq)

all_opts :: [OptDescr Options]
all_opts = concat [subcommand "" SystemOption sysopts] ++ fileopts

fileopts :: [OptDescr Options]
fileopts = concat [subcommand "" EnvmntOption envopts,
                   (subcommand <$> fst <*> return CStaticOption <*> snd) options,
                   (subcommand <$> fst <*> return GraphVizOption <*> snd) options]

printUsage :: IO ()
printUsage = putStr $ usageInfo header all_opts

printVersion :: IO ()
printVersion = putStrLn (app_name ++ " version: " ++
                         (showVersion $ packageVersion packageInfo))

rename :: String -> Either String (QualifiedName, QualifiedName)
rename s = case map (second reads) $ reads s of
            [(a, [(b, "")])] -> Right (a, b)
            otherwise      -> Left s

--processFile :: String -> [Options] -> IO ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable)
processFile fileName os = do
    compilationUnit <-
        if fileName == "-"
            then getContents
            else readFile fileName
    case parse (smudge_file <* eof) fileName compilationUnit of
        Left err -> print err >> report_failure 1
        Right (ps, sms) ->
            let flatten (a, b, c) = (concat a ++ os, concat b, concat c)
            in  case flatten $ unzip3 $ map (getOpt Permute fileopts) ps of
                    (os, [], []) -> checkAndConvert sms os >>= make_output outputTarget os
                    (_, fs, es) -> do putStr (concat es)
                                      mapM (putStrLn . ("unrecognized argument `" ++) . (++ "'")) fs
                                      report_failure (length fs + length es)
    where outputTarget = normalise $ joinPath [(prefix os), takeFileName fileName]
          prefix [] = dropFileName fileName
          prefix ((SystemOption (OutDir p)):_) = p
          prefix (_:t) = prefix t

--checkAndConvert :: [(StateMachine Identifier, WholeState Identifier)] -> [Options] -> IO ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable)
checkAndConvert sms os = do
    let rename_errors = lefts renames
    mapM (putStrLn . ("Parse error in rename flag: " ++)) rename_errors
    when (not $ null rename_errors) $ report_failure $ length rename_errors
    m sms
    where
        renames = map rename [r | EnvmntOption (Rename r) <- os]
        namespace = last $ "SMUDGE" : [n | EnvmntOption (Namespace n) <- os]
        aliases = merge (basisAlias namespace) $ fromList $ rights renames
        m sms = do
            let sms' = passInitialState sms
            let sms'' = passRename aliases $ passFullyQualify sms'
            let sms''' = passTagCategories sms''
            let fs = concat $ map name_passes sms'''
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            let basis = bindBasis aliases $ map fst sms''
            let st = if elem (EnvmntOption Strict) os
                     then elaborateMono basis sms'''
                     else elaboratePoly basis sms'''
            -- This is a bit of a hack around the definition of Passable
            let types = take 1 (zip (map fst sms''') (repeat st))
            let fs = concat $ map type_passes types
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            let gs = passWholeStateToGraph sms'''
            let fs = concat $ map make_passes gs
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            return (gs, aliases, st)

report_failure :: Int -> IO a
report_failure n = putStrLn ("Exiting with " ++ show n ++ " error" ++ (if n == 1 then "" else "s")) >> exitFailure

--make_output :: String -> [Options] -> ([(StateMachine, Gr EnterExitState Happening)], Alias, SymbolTable) -> IO ()
make_output fileName os gswst = do
    let gvos = [a | GraphVizOption a <- os]
    outputNames <- generate gvos gswst fileName
    mapM_ putStrLn $ do outputName <- outputNames
                        ["Wrote file \"" ++ outputName ++ "\""]
    let csos = [a | CStaticOption a <- os]
    outputNames <- generate csos gswst fileName
    mapM_ putStrLn $ do outputName <- outputNames
                        ["Wrote file \"" ++ outputName ++ "\""]

main = do
    args <- getArgs
    case getOpt Permute all_opts args of
        (os,             _, [])
            | elem (SystemOption Help) os -> printUsage
            | elem (SystemOption Version) os -> printVersion

        (os, (fileName:as),  _) -> processFile fileName os

        (_,              _, es) -> putStr (concat es) >> printUsage
