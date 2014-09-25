{-# LANGUAGE FlexibleInstances #-}
module Trashcan.Graph where

import Control.Arrow ((***))
import Data.Graph.Inductive.Graph (
    DynGraph,
    empty,
    gmap,
    insEdges,
    insNodes,
    labEdges,
    labNodes,
    newNodes,
    noNodes,
    nodes
    )
import Data.Map ((!), fromList)
import Data.Monoid (Monoid(..))

instance DynGraph gr => Monoid (gr a b) where
    mempty = empty
    mappend = grMerge

grMerge :: DynGraph gr => gr a b -> gr a b -> gr a b
grMerge f g = insEdges (labEdges g') $ insNodes (labNodes g') f
    where
        g'  = gmap ctc g
        ctc (i, n, l, o) = (map (id *** ntn) i, ntn n, l, map (id *** ntn) o)
        ntn = (!) $ fromList $ zip (nodes g) (newNodes (noNodes g) f)
