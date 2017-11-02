-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

module Trashcan.FilePath (
    relPath,
    normaliseParentDirs
) where

import Control.Applicative (
    pure,
    liftA,
    liftA2,
    (<$>),
    (<*>)
    )
import System.Directory (getCurrentDirectory)
import System.FilePath (
    dropTrailingPathSeparator,
    equalFilePath,
    makeRelative,
    normalise,
    takeDirectory,
    (</>),
    joinPath,
    splitDirectories,
    isDrive,
    )

parentDir :: FilePath
parentDir = ".."

isParent :: FilePath -> Bool
isParent = equalFilePath parentDir

normaliseParentDirs :: FilePath -> FilePath
normaliseParentDirs = fixInvalid . joinPath . filterParents . splitDirectories . normalise
    where
        fixInvalid "" = "."
        fixInvalid  x = x
        filterParents (  p:ds) | isParent p = parentDir:filterParents ds
        filterParents (a:p:ds) | isParent p = filterParents $ (if isDrive a then (a:) else id) ds
        filterParents (  a:ds)              = let ds' = filterParents ds in
                                               if ds' == ds then a:ds' else filterParents (a:ds')
        filterParents       []              = []

relPath :: FilePath -> FilePath -> IO FilePath
relPath x y = rp <$> dtps (npd (cd <</>> pure x)) <*> npd (cd <</>> pure y)
    where
        (=.=) = equalFilePath
        mr = makeRelative
        dir = takeDirectory
        dtps = liftA dropTrailingPathSeparator
        npd = liftA normaliseParentDirs
        (<</>>) = liftA2 (</>)
        cd = getCurrentDirectory
        rp a b | a =.= "."      = mr a b
        rp a b | b =.= (mr a b) = parentDir </> rp (dir a) b
        rp a b                  = mr a b
