-- Copyright 2018 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

module Main where

import Args(getAllOpt, getFileOpt)
import Args (
  Options(..),
  SystemOption(..),
  EnvmntOption(..),
  CommonOption(..),
  getAllOpt,
  getFileOpt,
  )
import Language.Smudge.Backends.Backend (
  generate,
  Config(..),
  defaultConfig,
  )
import Language.Smudge.Semantics.Model (
  EnterExitState,
  Happening,
  passInitialState,
  passFullyQualify,
  passRename,
  passTagCategories,
  passWholeStateToGraph,
  QualifiedName,
  qualify,
  Tagged(..),
  TaggedName,
  events_for,
  states_for,
  )
import Language.Smudge.Semantics.Operation (
  basicBlocks,
  )
import Language.Smudge.Grammar (
  StateMachine,
  WholeState,
  Event(..),
  State(..),
  )
import Language.Smudge.Semantics.Solver (
  SymbolTable,
  elaborateMono,
  elaboratePoly,
  )
import Language.Smudge.Parsers.Id (Identifier)
import Language.Smudge.Parsers.Smudge (smudge_file)
import Language.Smudge.Semantics.Alias (Alias, merge)
import Language.Smudge.Semantics.Basis (basisAlias, bindBasis)
import Language.Smudge.Passes.Passes (Severity(..), Fault(..), fatal)
import Language.Smudge.Passes (
  make_passes,
  name_passes,
  link_passes,
  type_passes,
  term_passes,
  )
import Data.Graph.Extra

import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Arrow (second)
import Control.Applicative ((<$>), (<*>))
import Text.Parsec (parse, eof)
import System.Environment (getArgs)
import System.FilePath (joinPath, takeFileName, dropFileName, normalise)
import System.Exit (exitFailure)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Either (lefts, rights, partitionEithers, isLeft)
import Data.List (partition)
import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Set (Set, member, insert, delete)
import qualified Data.Set as Set (fromList)

rename :: String -> Either String (QualifiedName, QualifiedName)
rename s = case map (second reads) $ reads s of
            [(a, [(b, "")])] -> Right (a, b)
            otherwise      -> Left s

readEither :: Read a => String -> Either String a
readEither s = case reads s of
            [(x, "")] -> Right x
            otherwise -> Left s

processFile :: String -> [Options] -> IO ()
processFile fileName os = do
    compilationUnit <-
        if fileName == "-"
            then getContents
            else readFile fileName
    case parse (smudge_file <* eof) fileName compilationUnit of
        Left err -> print err >> report_failure 1
        Right (ps, sms) -> do
            os' <- getFileOpt ps
            let os'' = os' ++ os
            converted <- checkAndConvert sms os''
            make_output outputTarget os'' converted
    where outputTarget = normalise $ joinPath [(prefix os), takeFileName fileName]
          prefix [] = dropFileName fileName
          prefix ((SystemOption (OutDir p)):_) = p
          prefix (_:t) = prefix t

checkAndConvert :: [(StateMachine Identifier, [WholeState Identifier])] -> [Options] -> IO ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable)
checkAndConvert sms os = do
    let rename_errors = lefts renames
    mapM (putStrLn . ("Parse error in rename flag: " ++)) rename_errors
    when (not $ null rename_errors) $ report_failure $ length rename_errors
    m sms
    where
        renames = map rename [r | EnvmntOption (Rename r) <- os]
        namespace = last $ "SMUDGE" : [n | EnvmntOption (Namespace n) <- os]
        aliases = merge (basisAlias namespace) $ fromList $ rights renames
        nsprefix = if EnvmntOption (NsPrefix True) `elem` os && not (null namespace)
                   then qualify . (,) namespace else id
        m sms = do
            let sms' = passInitialState sms
            let sms'' = passRename aliases nsprefix $ passFullyQualify sms'
            let sms''' = passTagCategories sms''
            let fs = concat $ map name_passes sms'''
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            -- This is evidence that the definition of Passable is broken
            let program = zip (map fst sms''') [sms''']
            let fs = concat $ map link_passes program
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            let basis = bindBasis aliases $ map fst sms''
            let st = if elem (EnvmntOption (Strict True)) os
                     then elaborateMono basis sms'''
                     else elaboratePoly basis sms'''
            -- This is a bit of a hack around the definition of Passable
            let types = zip (map fst sms''') [st]
            let fs = concat $ map type_passes types
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            let gs = passWholeStateToGraph sms'''
            let fs = concat $ map make_passes gs
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            let bs = map (second basicBlocks) gs
            let fs = concat $ map term_passes bs
            mapM print fs
            when (any fatal fs) $ report_failure $ length fs

            return (gs, aliases, st)

report_failure :: Int -> IO a
report_failure n = putStrLn ("Exiting with " ++ show n ++ " error" ++ (if n == 1 then "" else "s")) >> exitFailure

buildIdSet :: (Read a, Show a, Ord b) => (a -> b) -> [(Bool, Maybe String)] -> Set b -> String -> IO (Set b)
buildIdSet mk os universe flagName =
    let builder  True  Nothing = const universe
        builder  True (Just x) = insert x
        builder False  Nothing = const mempty
        builder False (Just x) = delete x
        parsed = [(,) b <$> traverse readEither x | (b, x) <- os]
        (errors, opts) = partitionEithers parsed
        (present, missing) = partition (all (`member` universe) . fmap mk . snd) opts
        builders = map (uncurry builder . second (fmap mk)) present
    in do
        mapM (putStrLn . (("Parse error in " ++ flagName ++ " flag: ") ++)) errors
        mapM (putStrLn . (("Missing name error in " ++ flagName ++ " flag: ") ++) . show . fromJust . snd) missing
        when (not $ null errors && null missing) $ report_failure $ length errors + length missing
        return $ foldl (flip ($)) mempty builders

make_output :: String -> [Options] -> ([(StateMachine TaggedName, Gr EnterExitState Happening)], Alias QualifiedName, SymbolTable) -> IO ()
make_output fileName os gswst@(gs, _, _) = do
    let cos = [a | CommonOption a <- os]
    let noDebug = elem (Debug False) cos
    let allEvents = Set.fromList $ concatMap (events_for . snd) gs
    logEvents <- buildIdSet (Event . TagEvent) [(b, x) | LogEvent b x <- cos] allEvents "logEvent"
    let allStates = Set.fromList $ concatMap (states_for . snd) gs
    logStates <- buildIdSet (State . TagState) [(b, x) | LogState b x <- cos] allStates "logState"
    let dbgCfg = if noDebug then defaultConfig { debug=False } else defaultConfig
    let config = dbgCfg { logEvent=logEvents, logState=logStates }
    let gvos = [a | GraphVizOption a <- os]
    gres <- runExceptT $ generate gvos config gswst fileName
    let csos = [a | CStaticOption a <- os]
    cres <- runExceptT $ generate csos config gswst fileName
    let fs = lefts [gres, cres]
    let outputNames = concat $ rights [gres, cres]
    mapM_ putStrLn $ do outputName <- outputNames
                        ["Wrote file \"" ++ outputName ++ "\""]
    mapM print fs
    when (any fatal fs) $ report_failure $ length fs

main = do
    args <- getArgs
    (os, (fileName:as)) <- getAllOpt args
    processFile fileName os
