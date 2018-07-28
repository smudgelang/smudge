-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

module Data.GraphViz.Attributes.Extra (
    labelCrlf,
    toLabelList,
    toTable,
    wrapLabel,
) where

import Control.Arrow (first, (***), (&&&))
import Data.Function (on)
import Data.GraphViz (Labellable(..))
import Data.GraphViz.Attributes.Complete (
    Label(..),
    RecordField(..),
    RankDir(..)
    )
import qualified Data.GraphViz.Attributes.HTML as H
import Data.List (intersperse, groupBy, sortBy, nub)
import qualified Data.Text.Lazy as T (Text, empty, lines, toStrict, fromStrict)
import Data.Word (Word16)
import GHC.Exts (the)
import Text.Wrap (wrapText, defaultWrapSettings)

instance Semigroup H.Table where
    (H.HTable f1 a1 r1) <> (H.HTable f2 a2 r2) =
        H.HTable (nub <$> f1 <> f2) (nub $ a1 <> a2) (r1 <> r2)

instance Monoid H.Table where
    mempty = H.HTable Nothing [] []
    mappend = (<>)

instance Semigroup H.Label where
    (H.Text a) <> (H.Text b) = H.Text $ a <> b
    a <> b = H.Table $ (toTable a) <> (toTable b)
        where toTable (H.Table t) = t
              toTable l = H.HTable Nothing [] [H.Cells [H.LabelCell [] l]]

instance Monoid H.Label where
    mempty = H.Text []
    mappend = (<>)

instance Semigroup Label where
    (StrLabel a) <> (StrLabel b) = StrLabel (a <> b)
    (RecordLabel a) <> (RecordLabel b) = RecordLabel (a <> b)
    (StrLabel a) <> (RecordLabel b) = RecordLabel ([FieldLabel a] <> b)
    (RecordLabel a) <> (StrLabel b) = RecordLabel (a <> [FieldLabel b])
    a <> b = HtmlLabel $ (toHtml a) <> (toHtml b)
        where toHtml (HtmlLabel l) = l
              toHtml (StrLabel l) = H.Text $ escapeNewlines l
              toHtml (RecordLabel fs) = H.Table $ toTable FromTop fs

instance Monoid Label where
    mempty = StrLabel T.empty
    mappend = (<>)

escapeNewlines :: T.Text -> H.Text
escapeNewlines = intersperse (H.Newline []) . map H.Str . T.lines

toTable :: RankDir -> [RecordField] -> H.Table
toTable dir = H.HTable Nothing [H.Border 0, H.CellBorder 1, H.CellPadding 8, H.CellSpacing 0] . map H.Cells . toAxis dir
    where
        height :: [RecordField] -> Word16
        height = foldl lcm 1 . map fheight
            where fheight (FlipFields fs) = width fs
                  fheight _ = 1

        width :: [RecordField] -> Word16
        width = sum . widths

        widths :: [RecordField] -> [Word16]
        widths = map fwidth
            where fwidth (FlipFields fs) = height fs
                  fwidth _ = 1

        scaled_widths :: Word16 -> [RecordField] -> [Word16]
        scaled_widths w rs = map (* (w `div` width rs)) $ widths rs

        toAxis :: RankDir -> [RecordField] -> [[H.Cell]]
        toAxis FromTop    = by col
        toAxis FromBottom = by col
        toAxis FromLeft   = by row
        toAxis FromRight  = by row

        by :: ([RecordField] -> Word16 -> Word16 -> [(Word16, [H.Cell])]) -> [RecordField] -> [[H.Cell]]
        by axis = intersperseEmptyRows 0 . (axis <*> width <*> height)
            where intersperseEmptyRows :: Word16 -> [(Word16, [H.Cell])] -> [[H.Cell]]
                  intersperseEmptyRows _           []            = []
                  intersperseEmptyRows y ((y', cs):rs) | y == y' = cs:intersperseEmptyRows (y + 1) rs
                  intersperseEmptyRows y           rs            = [emptyCell]:intersperseEmptyRows (y + 1) rs
                  emptyCell :: H.Cell
                  emptyCell = H.LabelCell [H.FixedSize True, H.Border 0, H.CellPadding 0, H.CellSpacing 0, H.Height 1, H.Width 1] mempty

        bySize :: [RecordField] -> Word16 -> Word16 -> [(RecordField, (Word16, Word16))]
        bySize rs w h = zip rs $ zip (scaled_widths w rs) $ repeat h

        asFields :: RecordField -> [RecordField]
        asFields (FlipFields fs) = fs
        asFields f = [f]

        row :: [RecordField] -> Word16 -> Word16 -> [(Word16, [H.Cell])]
        row rs w h = stackByOffset $ map (first (col . asFields)) $ bySize rs w h
            where stackByOffset = concatMap (uncurry (map . first . (+))) . (zip . scanl (+) 0 . map (fst . snd) <*> map (uncurry (uncurry . flip . ($))))

        col :: [RecordField] -> Word16 -> Word16 -> [(Word16, [H.Cell])]
        col rs w h = lineUpByOffset $ concatMap (uncurry $ uncurry . cells) $ bySize rs w h
            where lineUpByOffset = map ((the *** concat) . unzip) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

        cells :: RecordField -> Word16 -> Word16 -> [(Word16, [H.Cell])]
        cells (LabelledTarget p s) w h = [(0, [H.LabelCell [H.RowSpan h, H.ColSpan w, H.Port p] (H.Text $ escapeNewlines s)])]
        cells (PortName p)         w h = [(0, [H.LabelCell [H.RowSpan h, H.ColSpan w, H.Port p] mempty])]
        cells (FieldLabel s)       w h = [(0, [H.LabelCell [H.RowSpan h, H.ColSpan w] (H.Text $ escapeNewlines s)])]
        cells (FlipFields fs)      w h = row fs h w

labelCrlf :: Label
labelCrlf = toLabelValue "\n"

toLabelList :: Labellable a => String -> [a] -> Label
toLabelList sep = mconcat . intersperse (toLabelValue sep) . map toLabelValue

wrapLabel :: Int -> Label -> Label
wrapLabel w (StrLabel t) = StrLabel $ T.fromStrict $ wrapText defaultWrapSettings w $ T.toStrict t
wrapLabel _ l = l -- wrapping this is meaningless
