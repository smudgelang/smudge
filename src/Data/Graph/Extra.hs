-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

{-# LANGUAGE FlexibleInstances #-}

module Data.Graph.Extra (
    cycles
) where

import Control.Arrow ((***))
import Data.Graph.Inductive.Graph (
    DynGraph,
    Graph,
    Node,
    delNode,
    empty,
    gelem,
    gmap,
    insEdges,
    insNodes,
    isEmpty,
    labEdges,
    labNodes,
    newNodes,
    noNodes,
    nodes,
    suc
    )
import Data.Map ((!), fromList)
import Data.Monoid (Monoid(..))

instance {-# OVERLAPS #-} DynGraph gr => Monoid (gr a b) where
    mempty = empty
    mappend = grMerge

grMerge :: DynGraph gr => gr a b -> gr a b -> gr a b
grMerge f g = insEdges (labEdges g') $ insNodes (labNodes g') f
    where
        g'  = gmap ctc g
        ctc (i, n, l, o) = (map (id *** ntn) i, ntn n, l, map (id *** ntn) o)
        ntn = (!) $ fromList $ zip (nodes g) (newNodes (noNodes g) f)

cycles :: Graph gr => gr a b -> [[Node]]
cycles g | isEmpty g = []
cycles g             = cyclesFrom root g ++ cycles (delNode root g)
    where
        root = nodes g !! 0

        cyclesFrom :: Graph gr => Node -> gr a b -> [[Node]]
        cyclesFrom n g = concat $ map (elemCircuits n g n) (suc g n)

        elemCircuits :: Graph gr => Node -> gr a b -> Node -> Node -> [[Node]]
        elemCircuits s g p n | not $ gelem n g = if s == n then [[n]] else []
        elemCircuits s g p n | null (suc g n)  = if s == n then [[p, n]] else []
        elemCircuits s g p n = map (p:) $ concat $ map (elemCircuits s (delNode n g) n) (suc g n)
