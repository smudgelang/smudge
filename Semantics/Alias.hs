module Semantics.Alias (
    Alias,
    rename,
) where

import Data.Map (Map, findWithDefault)

type Alias a = Map a a

rename :: Ord a => Alias a -> a -> a
rename aliases a = findWithDefault a a aliases
