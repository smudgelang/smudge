module Main where

import Text.ParserCombinators.Parsec (parse, ParseError)
import System.Environment
import System.FilePath
import Grammar
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz hiding (parse)
import Data.GraphViz.Attributes.Complete (Label(..))
import Data.Text.Lazy.Internal (Text(..))
import Data.Monoid
import Data.List (intersperse)
import qualified Data.Map as Map

-- Sinful.  This instance is incomplete.
instance Monoid Label where
    mempty = StrLabel Empty
    mappend (StrLabel a) (StrLabel b) = StrLabel (mappend a b)

instance Labellable State where
    toLabelValue (State s) = toLabelValue s
    toLabelValue s         = toLabelValue $ show s

instance Labellable Event where
    toLabelValue (Event e) = toLabelValue e
    toLabelValue e         = toLabelValue $ show e

instance Labellable SideEffect where
    toLabelValue (FuncVoid f) = toLabelValue f
    toLabelValue (FuncEvent f (s, e)) = toLabelValue e
    toLabelValue (FuncDefault (s, e)) = toLabelValue e

data EventAndSideEffects = EventAndSideEffects Event [SideEffect]
    deriving (Show, Eq, Ord)

instance Labellable EventAndSideEffects where
    toLabelValue (EventAndSideEffects e ses) = mconcat $ intersperse (toLabelValue "\n") $ toLabelValue e : map toLabelValue ses

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

labelNonClusteredParams = nonClusteredParams { fmtNode = \ (_, l) -> [toLabel l],
                                               fmtEdge = \ (_, _, l) -> [toLabel l] }

main = do
    args <- getArgs
    let fileName = head args
    compilationUnit <- readFile fileName
    case parse state_machine fileName compilationUnit of
        Left err -> print err
        Right sm -> do let g = smToGraph sm
                       let outputFileName = (dropExtension fileName) ++ ".svg"
                       let d = graphToDot labelNonClusteredParams g
                       runGraphviz d Svg outputFileName
                       print $ labNodes g
                       print $ labEdges g
