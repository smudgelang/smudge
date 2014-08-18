module Main where

import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Environment
import Grammar
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Map as Map

smToGraph :: (StateMachine, [(State, [(Event, [SideEffect], State)])]) -> Gr State (Event, [SideEffect])
smToGraph (sm, ss) = mkGraph [s | s <- zip [1..] (map fst ss)] es
                     where sn = Map.fromList [s | s <- zip (map fst ss) [1..]]
                           es = [ese | ese <- concat $ map (\ (s, es) -> 
                                                            map (\ (e, ses, s') ->
                                                                 let s'' = case s' of
                                                                                StateSame -> s
                                                                                otherwise -> s'
                                                                 in (sn Map.! s, sn Map.! s'', (e, ses))) es) ss]

main = do
    args <- getArgs
    let fileName = head args
    compilationUnit <- readFile fileName
    case parse state_machine fileName compilationUnit of
        Left err -> print err
        Right sm -> do let g = smToGraph sm
                       print $ labNodes g
                       print $ labEdges g
