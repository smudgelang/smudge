{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Backends.GraphViz where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), Happening(..), EnterExitState(..))

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
    toLabelValue s         = toLabelValue $ show s

decorate :: EnterExitState -> String
decorate (en, s, ex) =
    let efl l e = case l of
            Nothing -> ""
            Just ens -> "\n" ++ intercalate "\n" (e:(map show ens))
        s' = case s of
            StateAny -> "Any"
            State sl -> sl
    in s' ++ (efl en "Enter:") ++ (efl ex "Exit:")

instance Labellable QualifiedState where
    toLabelValue (_, qs) = toLabelValue $ decorate qs

instance Labellable Event where
    toLabelValue (Event e) = toLabelValue e
    toLabelValue e         = toLabelValue $ show e

instance Labellable SideEffect where
    toLabelValue (FuncVoid f) = toLabelValue f
    toLabelValue (FuncEvent f (s, e)) = toLabelValue e
    toLabelValue (FuncDefault (s, e)) = mconcat $ toLabelValue s : [toLabelValue e]

instance Labellable Happening where
    toLabelValue (Hustle e ses) = mconcat $ intersperse (toLabelValue "\n") $ toLabelValue e : map toLabelValue ses
    toLabelValue (Bustle e ses) = mconcat $ intersperse (toLabelValue "\n") $ toLabelValue e : map toLabelValue ses

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
            drop (start, end, ese@(Hustle e _)) = [toLabel e, arrow ese]
            drop (start, end, ese@(Bustle e _)) = [toLabel e, arrow ese]
            arrow (Hustle _ _) = edgeEnds Forward
            arrow (Bustle _ _) = edgeEnds NoDir

data GraphVizOption = Format GraphvizOutput | OutFile FilePath | SuppressSideEffects
    deriving (Show, Eq)

outputFormats :: [GraphvizOutput]
outputFormats = [minBound..maxBound]

gfold :: [(StateMachine, UnqualifiedGraph)] -> QualifiedGraph
gfold = foldl gf G.empty
    where
        gf :: QualifiedGraph -> (StateMachine, UnqualifiedGraph) -> QualifiedGraph
        gf qg (sm, ug) = G.ufold (qualifyAndRename sm nameMap) qg ug
            where
                nameMap :: NodeMap
                nameMap =
                    let
                        before :: [G.Node]
                        before = [fst (G.nodeRange ug)..snd (G.nodeRange ug)]
                        after :: [G.Node]
                        after = G.newNodes (length before) qg
                    in M.fromList $ zip before after
                qualifyAndRename :: StateMachine -> NodeMap ->
                    UnqualifiedContext -> QualifiedGraph -> QualifiedGraph
                qualifyAndRename sm nm c acc =
                    let
                        (ins, n, l, outs) = c
                        renameAdj :: (l, G.Node) -> (l, G.Node)
                        renameAdj (lbl, n) = (lbl, nameMap M.! n)
                        rins = map renameAdj ins
                        routs = map renameAdj outs
                        rn = nameMap M.! n
                        ql = (sm, l)
                        done :: QualifiedContext
                        done = (rins, rn, ql, routs)
                    in done G.& acc

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
