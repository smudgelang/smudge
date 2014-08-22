module Backends.GraphViz where

import Backends.Backend
import Grammar

import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Label(..))
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

instance Labellable EventAndSideEffects where
    toLabelValue (EventAndSideEffects e ses) = mconcat $ intersperse (toLabelValue "\n") $ toLabelValue e : map toLabelValue ses

labelNonClusteredParams = nonClusteredParams { fmtNode = \ (_, l) -> [toLabel l]
                                             , fmtEdge = \ (_, _, l) -> [toLabel l] }

labelNonClusteredParamsNoSE = nonClusteredParams { fmtNode = \ (_, l) -> [toLabel l]
                                                 , fmtEdge = \ (_, _, EventAndSideEffects e _) -> [toLabel e] }

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
    generate os g inputName = addExtension (runGraphviz (d os)) format (dropExtension inputName)
        where d os 
                | SuppressSideEffects `elem` os = graphToDot labelNonClusteredParamsNoSE g
                | otherwise = graphToDot labelNonClusteredParams g
              format = formatHelper os
              formatHelper [] = DotOutput
              formatHelper ((Format f):_) = f
              formatHelper (_:os) = formatHelper os
