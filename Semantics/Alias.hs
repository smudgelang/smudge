module Semantics.Alias (
    Alias,
    merge,
    rename,
) where

import Data.Map (Map, union, findWithDefault)
import qualified Data.Map as Map (map)

type Alias a = Map a a

merge :: Ord a => Alias a -> Alias a -> Alias a
merge a b = union b (Map.map (rename b) a)

rename :: Ord a => Alias a -> a -> a
rename aliases a = findWithDefault a a aliases
