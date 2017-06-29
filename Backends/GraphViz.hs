{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Backends.GraphViz (
    GraphVizOption(..)
) where

import Backends.Backend (Backend(..))
import Grammars.Smudge (
  StateMachine(..),
  State(..),
  Event(..),
  Function(..),
  SideEffect(..),
  )
import Model (
  TaggedName,
  disqualifyTag,
  HappeningFlag(..),
  Happening(..),
  EnterExitState(..)
  )
import Trashcan.Graph
import Trashcan.GetOpt (OptDescr(..), ArgDescr(..))

import Data.GraphViz (
  GraphvizParams(..),
  GraphvizOutput(..),
  Labellable(..),
  NodeCluster(..),
  GlobalAttributes(..),
  ToGraphID(..),
  DotGraph(..),
  defaultParams,
  runGraphviz,
  graphToDot,
  addExtension,
  )
import Data.GraphViz.Attributes (
  toLabel,
  shape,
  style,
  filled,
  invis,
  edgeEnds,
  fillColor,
  Shape(Circle),
  X11Color(Black),
  DirType(Forward, NoDir),
  )
import Data.GraphViz.Attributes.Complete (Label(..), Attribute(Concentrate))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as M
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate, intersperse)
import Data.Text.Internal.Lazy (Text(..))
import Control.Monad.Trans (liftIO)
import System.FilePath (FilePath, dropExtension)

type QualifiedState = (StateMachine TaggedName, EnterExitState)
type UnqualifiedGraph = Gr EnterExitState Happening
type QualifiedGraph = Gr QualifiedState Happening
type NodeMap = M.Map G.Node G.Node
type QualifiedContext = G.Context QualifiedState Happening
type UnqualifiedContext = G.Context EnterExitState Happening


-- Sinful.  This instance is incomplete.
instance Monoid Label where
    mempty = StrLabel Empty
    mappend (StrLabel a) (StrLabel b) = StrLabel (mappend a b)

instance Labellable TaggedName where
    toLabelValue = toLabelValue . disqualifyTag

instance Labellable (StateMachine TaggedName) where
    toLabelValue (StateMachine sm) = toLabelValue sm
    toLabelValue s                 = toLabelValue $ show s

instance Labellable (State TaggedName) where
    toLabelValue (State s) = toLabelValue s
    toLabelValue StateAny  = toLabelValue "Any"
    toLabelValue StateSame = toLabelValue "Same"

labelCrlf :: Label
labelCrlf = toLabelValue "\n"

instance Labellable EnterExitState where
    toLabelValue EnterExitState {en, st, ex} = mconcat $ intersperse labelCrlf $ toLabelValue st : efl en "Enter:" ++ efl ex "Exit:"
        where efl l e = (take 1 (l >> [toLabelValue e])) ++ map toLabelValue l

instance Labellable QualifiedState where
    toLabelValue (_, qs) = toLabelValue qs

instance Labellable (Event TaggedName) where
    toLabelValue (Event e) = toLabelValue e
    toLabelValue e         = toLabelValue $ show e

instance Labellable (SideEffect TaggedName) where
    toLabelValue (f, FuncVoid) = toLabelValue f
    toLabelValue (f, FuncTyped (s, e)) = toLabelValue e
    toLabelValue (f, FuncEvent (s, e)) = mconcat [toLabelValue s, toLabelValue e]

instance Labellable Happening where
    toLabelValue (Happening e ses _) = mconcat $ intersperse labelCrlf $ toLabelValue e : map toLabelValue ses

smudgeParams sideEffects noTransitions clusterBox title entryNodes =
    defaultParams
        { globalAttributes =
                [GraphAttrs [toLabel title, Concentrate True]]
        , clusterBy = cluster
        , isDotCluster = const clusterBox
        , clusterID = toGraphID
        , fmtNode = fmtNode
        , fmtEdge = if sideEffects then keep else drop
        , fmtCluster = clusterAttrs
        }
        where
            cluster (n, nl@(sm, _)) = C (smToString sm) (N (n, nl))
            clusterAttrs c = [GraphAttrs [toLabel c, Concentrate True]]
            smToString (StateMachine s) = disqualifyTag s
            fmtNode (_, (_, EnterExitState {st = StateEntry})) = [shape Circle, style filled, fillColor Black, toLabel ""]
            fmtNode (_, l) = [toLabel l]
            keep (n, _, ese) = [filtEntry n (toLabel ese), arrow ese]
            drop (n, _, ese) = [filtEntry n (toLabel $ event ese), arrow ese]
            filtEntry n l = if n `elem` entryNodes then toLabel "" else l
            arrow (Happening _ _ [])                        = edgeEnds Forward
            arrow (Happening _ _ fs) | elem NoTransition fs = if noTransitions then edgeEnds NoDir else style invis

data GraphVizOption = Format GraphvizOutput | OutFile FilePath | RenderSideEffects Bool | SuppressNoTransition
    deriving (Show, Eq)

outputFormats :: [GraphvizOutput]
outputFormats = [Eps, Bmp, Canon, DotOutput, Eps, Fig, Gd, Gd2, Gif, Ico,
                 Imap, Cmapx, ImapNP, CmapxNP, Jpeg, Pdf, Plain, Png,
                 Ps, Ps2, Svg, SvgZ, Tiff, Vml, VmlZ, Vrml, WBmp]

gfold :: [(StateMachine TaggedName, UnqualifiedGraph)] -> QualifiedGraph
gfold = mconcat . (map qualify)
    where
        qualify :: (StateMachine TaggedName, UnqualifiedGraph) -> QualifiedGraph
        qualify (sm, ug) = G.gmap (\ (i, n, l, o) -> (i, n, (sm, l), o)) ug

instance Backend GraphVizOption where
    options = ("dot",
               [Option [] ["fmt"] (ReqArg (Format . read) "FORMAT")
                 ("GraphViz output format.  Options: " ++
                  (intercalate "," $ map show outputFormats)),
                Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["no-se"] (BoolArg RenderSideEffects)
                 "Suppress side effects from output",
                Option [] ["suppress-nontransition"] (NoArg SuppressNoTransition)
                 "Suppress non-transitioning events from output."])

    generate os cfg gswust inputName = liftIO $ sequence [runner (runGraphviz d) format outputName]
        where d = (graphToDot (smudgeParams renderSE renderNT (length gs > 1) inputName entryNodes) g') {graphID = Just (toGraphID " ")}
              g' = gfold gs
              (gs, _, _) = gswust
              entryNodes = [n | (n, (_, EnterExitState {st = StateEntry})) <- G.labNodes g']
              renderSE = not $ RenderSideEffects False `elem` os
              renderNT = not $ SuppressNoTransition `elem` os

              format = head $ [f | Format f <- os] ++ [DotOutput]
              outputName = head $ [f | OutFile f <- os] ++ [dropExtension inputName]
              runner = head $ [id | OutFile _ <- os] ++ [addExtension]
