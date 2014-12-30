{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Backends.GraphViz where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..))
import Model (HappeningFlag(..), Happening(..), EnterExitState(..))
import Trashcan.Graph

import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Label(..))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as M
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate, intersperse)
import Data.Monoid
import Data.Text.Lazy.Internal (Text(..))
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension)

type QualifiedState = (StateMachine, EnterExitState)
type UnqualifiedGraph = Gr EnterExitState Happening
type QualifiedGraph = Gr QualifiedState Happening
type NodeMap = M.Map G.Node G.Node
type QualifiedContext = G.Context QualifiedState Happening
type UnqualifiedContext = G.Context EnterExitState Happening


-- Sinful.  This instance is incomplete.
instance Monoid Label where
    mempty = StrLabel Empty
    mappend (StrLabel a) (StrLabel b) = StrLabel (mappend a b)

instance Labellable StateMachine where
    toLabelValue (StateMachine sm) = toLabelValue sm
    toLabelValue s                 = toLabelValue $ show s

instance Labellable State where
    toLabelValue (State s) = toLabelValue s
    toLabelValue StateAny  = toLabelValue "Any"
    toLabelValue StateSame = toLabelValue "Same"

labelCrlf :: Label
labelCrlf = toLabelValue "\n"

instance Labellable EnterExitState where
    toLabelValue (en, s, ex) = mconcat $ intersperse labelCrlf $ toLabelValue s : efl en "Enter:" ++ efl ex "Exit:"
        where efl l e = (take 1 (l >> [toLabelValue e])) ++ map toLabelValue l

instance Labellable QualifiedState where
    toLabelValue (_, qs) = toLabelValue qs

instance Labellable Event where
    toLabelValue (Event e) = toLabelValue e
    toLabelValue e         = toLabelValue $ show e

instance Labellable SideEffect where
    toLabelValue (FuncVoid f) = toLabelValue f
    toLabelValue (FuncEvent f (s, e)) = toLabelValue e
    toLabelValue (FuncDefault (s, e)) = mconcat [toLabelValue s, toLabelValue e]

instance Labellable Happening where
    toLabelValue (Happening e ses _) = mconcat $ intersperse labelCrlf $ toLabelValue e : map toLabelValue ses

smudgeParams sideEffects clusterBox title=
    defaultParams
        { globalAttributes =
                [GraphAttrs [toLabel title]]
        , clusterBy = cluster
        , isDotCluster = const clusterBox
        , clusterID = toGraphID
        , fmtNode = \ (_, l) -> [toLabel l]
        , fmtEdge = if sideEffects then keep else drop
        , fmtCluster = clusterAttrs
        }
        where
            cluster (n, nl@(sm, _)) = C (smToString sm) (N (n, nl))
            clusterAttrs c = [GraphAttrs [toLabel c]]
            smToString (StateMachine s) = s
            keep (_, _, l) = [toLabel l, arrow l]
            drop (start, end, ese) = [toLabel (event ese), arrow ese]
            arrow (Happening _ _ [])                        = edgeEnds Forward
            arrow (Happening _ _ fs) | elem NoTransition fs = edgeEnds NoDir

data GraphVizOption = Format GraphvizOutput | OutFile FilePath | SuppressSideEffects
    deriving (Show, Eq)

outputFormats :: [GraphvizOutput]
outputFormats = [minBound..maxBound]

gfold :: [(StateMachine, UnqualifiedGraph)] -> QualifiedGraph
gfold = mconcat . (map qualify)
    where
        qualify :: (StateMachine, UnqualifiedGraph) -> QualifiedGraph
        qualify (sm, ug) = G.gmap (\ (i, n, l, o) -> (i, n, (sm, l), o)) ug

instance Backend GraphVizOption where
    options = ("dot",
               [Option [] ["fmt"] (ReqArg (Format . read) "FORMAT")
                 ("GraphViz output format.  Options: " ++
                  (intercalate "," $ map show outputFormats)),
                Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["no-se"] (NoArg SuppressSideEffects)
                 "Suppress side effects from output"])

    generate os gs inputName = sequence [(runner os) (runGraphviz d) (format os) (outputName os)]
        where d = (graphToDot (smudgeParams suppressSE (length gs > 1) inputName) (gfold gs)) {graphID = Just (toGraphID " ")}
              suppressSE = not $ SuppressSideEffects `elem` os

              getFirstOrDefault :: ([a] -> b) -> b -> [a] -> b
              getFirstOrDefault _ d     [] = d
              getFirstOrDefault f _ (x:xs) = f xs
              format ((Format f):_) = f
              format xs = getFirstOrDefault format DotOutput xs
              outputName ((OutFile f):_) = f
              outputName xs = getFirstOrDefault outputName (dropExtension inputName) xs
              runner ((OutFile _):_) = id
              runner xs = getFirstOrDefault runner addExtension xs
