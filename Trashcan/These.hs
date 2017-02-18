module Trashcan.These (
    These(..),
    maybeThis,
    maybeThat,
    theseThis,
    theseThat,
    theseAndThis,
    theseAndThat,
    fmapThis,
    fmapThat,
) where

data These a b = This a | That b | These a b
    deriving (Eq, Show)

maybeThis :: These a b -> Maybe a
maybeThis = theseThis Nothing Just

maybeThat :: These a b -> Maybe b
maybeThat = theseThat Nothing Just

theseThis :: c -> (a -> c) -> These a b -> c
theseThis _ f (This a)    = f a
theseThis c _ (That _)    = c
theseThis _ f (These a _) = f a

theseThat :: c -> (b -> c) -> These a b -> c
theseThat c _ (This _)    = c
theseThat _ f (That b)    = f b
theseThat _ f (These _ b) = f b

theseAndThis :: c -> (a -> c) -> These a b -> These c b
theseAndThis c _ (That b) = These c b
theseAndThis _ f this     = fmapThis f this

theseAndThat :: c -> (b -> c) -> These a b -> These a c
theseAndThat c _ (This a) = These a c
theseAndThat _ f that     = fmapThat f that

fmapThis :: (a -> c) -> These a b -> These c b
fmapThis f (This a)     = This (f a)
fmapThis _ (That b)     = That b
fmapThis f (These a b)  = These (f a) b

fmapThat :: (b -> c) -> These a b -> These a c
fmapThat _ (This a)     = This a
fmapThat f (That b)     = That (f b)
fmapThat f (These a b)  = These a (f b)
