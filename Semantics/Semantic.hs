module Semantics.Semantic where (
    Passable(..)
)

import Data.Graph.Inductive.Graph

class Monoid a => Passable a where
    accumulate :: Context c d -> a -> a
    test :: a -> Bool
    pass :: Gr c d e -> Bool

    pass = test . (ufold accumulate mempty)
