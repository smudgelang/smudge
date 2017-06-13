module Args (
    Options(..),
    SystemOption(..),
    EnvmntOption(..),
    CommonOption(..),
    getAllOpt,
    getFileOpt
) where

import PackageInfo (version, buildCommit, appName, author, synopsis)
import Backends.Backend (options)
import Backends.GraphViz (GraphVizOption(..))
import Backends.CStatic (CStaticOption(..))

import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(Permute))
import System.Exit (exitSuccess, exitFailure)

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

data CommonOption = LogEvent |
                    NoDebug
    deriving (Show, Eq)

header :: String
header = "Usage: " ++ appName ++ " [OPTIONS] file\n" ++
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

cmnopts :: [OptDescr CommonOption]
cmnopts = [Option []    ["logevent"] (NoArg LogEvent) "Enable event tracing.",
           Option []    ["c-no-debug"] (NoArg NoDebug)
                         "Don't generate debugging information."]

data Options = SystemOption SystemOption | EnvmntOption EnvmntOption | CommonOption CommonOption | GraphVizOption GraphVizOption | CStaticOption CStaticOption
    deriving (Show, Eq)

all_opts :: [OptDescr Options]
all_opts = concat [subcommand "" SystemOption sysopts] ++ fileopts

fileopts :: [OptDescr Options]
fileopts = concat [subcommand "" EnvmntOption envopts,
                   subcommand "" CommonOption cmnopts,
                   (subcommand <$> fst <*> return CStaticOption <*> snd) options,
                   (subcommand <$> fst <*> return GraphVizOption <*> snd) options]

printUsage :: IO ()
printUsage = putStr (usageInfo header all_opts)

printVersion :: IO ()
printVersion = putStrLn (appName ++ " version: " ++ version)
            >> putStrLn ("build commit: " ++ buildCommit)

getAllOpt :: [String] -> IO ([Options], [String])
getAllOpt args =
    case getOpt Permute all_opts args of
        (os, _, [])
            | elem (SystemOption Help) os -> printUsage >> exitSuccess
            | elem (SystemOption Version) os -> printVersion >> exitSuccess
        (_, fs, es)
            | null fs -> putStrLn "No input files" >> putStr (concat es) >> printUsage >> exitFailure
            | not $ null es -> putStr (concat es) >> printUsage >> exitFailure
        (os, fs, _) -> return (os, fs)

getFileOpt :: [[String]] -> IO [Options]
getFileOpt argss =
    let flatten (a, b, c) = (concat a, concat b, concat c)
    in  case flatten $ unzip3 $ map (getOpt Permute fileopts) argss of
            (os, [], []) -> return os
            (_, fs, es) -> do putStr (concat es)
                              mapM (putStrLn . ("unrecognized argument `" ++) . (++ "'")) fs
                              exitFailure
