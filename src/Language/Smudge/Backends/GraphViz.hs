-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Smudge.Backends.GraphViz (
    GraphVizOption(..)
) where

import Language.Smudge.Backends.Backend (Backend(..))
import Language.Smudge.Grammar (
  StateMachine(..),
  State(..),
  Event(..),
  Function(..),
  SideEffect(..),
  )
import Language.Smudge.Semantics.Model (
  TaggedName,
  disqualifyTag,
  HappeningFlag(..),
  Happening(..),
  EnterExitState(..)
  )
import Data.Graph.Extra
import Data.GraphViz.Attributes.Extra (labelCrlf, toLabelList, wrapLabel)
import System.Console.GetOpt.Extra (OptDescr(..), ArgDescr(..))
import Language.Smudge.Passes.Passes (Fault(..), Severity(..))

import Data.GraphViz (
  GraphvizParams(..),
  GraphvizOutput(..),
  Labellable(..),
  NodeCluster(..),
  GlobalAttributes(..),
  ToGraphID(..),
  DotGraph(..),
  defaultParams,
  isGraphvizInstalled,
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
import Data.GraphViz.Attributes.Complete (Attribute(Concentrate))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as M
import Data.Function (on)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intercalate, intersperse, groupBy, sortBy)
import GHC.Exts (the)
import Control.Arrow (second)
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)
import Control.Exception (try, SomeException(..))
import System.FilePath (FilePath, dropExtension)

type QualifiedState = (StateMachine TaggedName, EnterExitState)
type UnqualifiedGraph = Gr EnterExitState Happening
type QualifiedGraph = Gr QualifiedState [Happening]


instance Labellable TaggedName where
    toLabelValue = toLabelValue . disqualifyTag

instance Labellable (StateMachine TaggedName) where
    toLabelValue (StateMachine sm) = toLabelValue sm
    toLabelValue s                 = toLabelValue $ show s

instance Labellable (State TaggedName) where
    toLabelValue (State s) = toLabelValue s
    toLabelValue StateAny  = toLabelValue "Any"
    toLabelValue StateSame = toLabelValue "Same"

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

instance Labellable [Happening] where
    toLabelValue = toLabelList "\n\n"

instance Labellable [Event TaggedName] where
    toLabelValue = wrapLabel 40 . toLabelList ", "

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
            fmt tl (n, _, eses) = (filtEntry n $ tl eses):map arrow (take 1 eses)
            keep = fmt toLabel
            drop = fmt (toLabel . map event)
            filtEntry n l = if n `elem` entryNodes then toLabel "" else l
            arrow (Happening _ _ [])                        = edgeEnds Forward
            arrow (Happening _ _ fs) | elem NoTransition fs = if noTransitions then edgeEnds NoDir else style invis

data GraphVizOption = Format GraphvizOutput | OutFile FilePath | RenderSideEffects Bool | SuppressNoTransition
    deriving (Show, Eq)

outputFormats :: [GraphvizOutput]
outputFormats = [Bmp, Canon, DotOutput, Eps, Fig, Gd, Gd2, Gif, Ico, Imap,
                 Cmapx, ImapNP, CmapxNP, Jpeg, Pdf, Plain, PlainExt, Png,
                 Ps, Ps2, Svg, SvgZ, Tiff, Vml, VmlZ, Vrml, WBmp, WebP]

gfold :: [(StateMachine TaggedName, UnqualifiedGraph)] -> QualifiedGraph
gfold = mconcat . (map qualify)
    where
        qualify :: (StateMachine TaggedName, UnqualifiedGraph) -> QualifiedGraph
        qualify (sm, ug) = G.gmap (\ (i, n, l, o) -> (condense i, n, (sm, l), condense o)) ug
        condense :: G.Adj Happening -> G.Adj [Happening]
        condense = map (second the . unzip) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)

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

    generate os cfg gswust inputName =
        ExceptT $ do result <- try $ sequence [runner (runGraphviz d) format outputName]
                     case result of
                        Left e -> friendlyMessage e >>= return . Left
                        Right r -> return $ Right r
        where d = (graphToDot (smudgeParams renderSE renderNT (length gs > 1) inputName entryNodes) g') {graphID = Just (toGraphID " ")}
              g' = gfold gs
              (gs, _, _) = gswust
              entryNodes = [n | (n, (_, EnterExitState {st = StateEntry})) <- G.labNodes g']
              renderSE = not $ RenderSideEffects False `elem` os
              renderNT = not $ SuppressNoTransition `elem` os

              format = head $ [f | Format f <- os] ++ [DotOutput]
              outputName = head $ [f | OutFile f <- os] ++ [dropExtension inputName]
              runner = head $ [id | OutFile _ <- os] ++ [addExtension]

              friendlyMessage (SomeException e) =
                do installed <- isGraphvizInstalled
                   return $ RuntimeFault ERROR $ if installed then show e else "GraphViz not found.  Please install GraphViz."
