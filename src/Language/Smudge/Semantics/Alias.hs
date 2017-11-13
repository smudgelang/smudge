-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

module Language.Smudge.Semantics.Alias (
    Alias,
    merge,
    rename,
    traverseAliases,
) where

import Data.Map (Map, union, findWithDefault)
import qualified Data.Map as Map (map, fromList, toList)

type Alias a = Map a a

merge :: Ord a => Alias a -> Alias a -> Alias a
merge a b = union b (Map.map (rename b) a)

rename :: Ord a => Alias a -> a -> a
rename aliases a = findWithDefault a a aliases

traverseAliases :: (Ord a, Ord b, Applicative f) => (a -> f b) -> Alias a -> f (Alias b)
traverseAliases f aliases = Map.fromList <$> traverse (\(k, v) -> (,) <$> f k <*> f v) (Map.toList aliases)
