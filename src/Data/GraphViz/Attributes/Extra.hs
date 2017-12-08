-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

module Data.GraphViz.Attributes.Extra (
    labelCrlf,
    toLabelList,
) where

import Data.GraphViz (Labellable(..))
import Data.GraphViz.Attributes.Complete (Label(..))
import Data.List (intersperse)
import Data.Text.Internal.Lazy (Text(..))

-- Sinful.  This instance is incomplete.
instance Monoid Label where
    mempty = StrLabel Empty
    mappend (StrLabel a) (StrLabel b) = StrLabel (mappend a b)

labelCrlf :: Label
labelCrlf = toLabelValue "\n"

toLabelList :: Labellable a => String -> [a] -> Label
toLabelList sep = mconcat . intersperse (toLabelValue sep) . map toLabelValue
