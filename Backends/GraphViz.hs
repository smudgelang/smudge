module Backends.GraphViz where

import Backends.Backend (Backend(..))
import Grammars.Smudge (StateMachine(..), State(..), Event(..), SideEffect(..), Happening(..))

import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Label(..))

import Data.Graph.Inductive.Graph (gmap)
import Data.List (intercalate, intersperse)
import Data.Monoid
import Data.Text.Lazy.Internal (Text(..))
import System.Console.GetOpt
import System.FilePath (FilePath, dropExtension)

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

labelNonClusteredParams sideEffects =
                         nonClusteredParams { globalAttributes =
                              [GraphAttrs [toLabel ""]] -- Placeholder
                          , fmtNode = \ (_, l) -> [toLabel l]
                          , fmtEdge = if sideEffects then keep else drop }
                          where
                                keep (_, _, l) = [toLabel l, arrow l]
                                drop (start, end, ese@(Hustle e _)) = [toLabel e, arrow ese]
                                arrow (Hustle _ _) = edgeEnds Forward
                                arrow (Bustle _ _) = edgeEnds NoDir

data GraphVizOption = Format GraphvizOutput | OutFile FilePath | SuppressSideEffects
    deriving (Show, Eq)

outputFormats :: [GraphvizOutput]
outputFormats = [minBound..maxBound]

instance Backend GraphVizOption where
    options = ("dot",
               [Option [] ["fmt"] (ReqArg (Format . read) "FORMAT")
                 ("GraphViz output format.  Options: " ++
                  (intercalate "," $ map show outputFormats)),
                Option [] ["o"] (ReqArg OutFile "FILE")
                 "The name of the target file if not derived from source file.",
                Option [] ["no-se"] (NoArg SuppressSideEffects)
                 "Suppress side effects from output"])

    generate os g inputName = (runner os) (runGraphviz d) (format os) (outputName os)
        where d = (graphToDot (labelNonClusteredParams suppressSE) g) {graphID = Just (toGraphID " ")}
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
