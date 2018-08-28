-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Package (packageVersion, packageName, PackageIdentifier(..), unPackageName)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..), Args)
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withExeLBI)
import Distribution.Simple.Setup (BuildFlags(..), fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFileEx)
import System.FilePath ((</>), (<.>))
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Distribution.Version (showVersion)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

main = defaultMainWithHooks packageInfoUserHooks

packageInfoUserHooks :: UserHooks
packageInfoUserHooks =
    simpleUserHooks {
        buildHook = genPackageInfoHook
    }

app_name :: PackageIdentifier -> String
app_name packageInfo = unPackageName $ packageName packageInfo

try_commands :: String -> [(String, [String])] -> IO String
try_commands def [] = return def
try_commands def ((cmd, args):cmds) = do
    (code, out, _) <- readProcessWithExitCode cmd args ""
    if code == ExitSuccess then return out else try_commands def cmds

build_commit :: IO String
build_commit = do
    out <- try_commands "UNKNOWN" [("git", ["rev-parse", "--short=12", "HEAD"]), ("hg", ["id", "-i"])]
    return $ dropWhileEnd isSpace out

genPackageInfoHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
genPackageInfoHook pkg lbi uhs bfs = do
    withExeLBI pkg lbi $ \_ clbi -> do
        createDirectoryIfMissingVerbose (fromFlag $ buildVerbosity bfs) True (autogenComponentModulesDir lbi clbi)
        let packageInfoModulePath = autogenComponentModulesDir lbi clbi </> cfg_name <.> "hs"
        rewriteFileEx (fromFlag $ buildVerbosity bfs) packageInfoModulePath . generate =<< build_commit
        buildHook simpleUserHooks pkg lbi uhs bfs
    where cfg_name = "PackageInfo"
          generate cmt = "module " ++ cfg_name ++ " where\n" ++
                         "\n" ++
                         "version     = " ++ (show $ showVersion $ packageVersion $ package pkg) ++ "\n" ++
                         "buildCommit = " ++ (show $ cmt) ++ "\n" ++
                         "appName     = " ++ (show $ app_name $ package pkg) ++ "\n" ++
                         "copyright   = " ++ (show $ copyright pkg) ++ "\n" ++
                         "maintainer  = " ++ (show $ maintainer pkg) ++ "\n" ++
                         "author      = " ++ (show $ author pkg) ++ "\n" ++
                         "stability   = " ++ (show $ stability pkg) ++ "\n" ++
                         "homepage    = " ++ (show $ homepage pkg) ++ "\n" ++
                         "pkgUrl      = " ++ (show $ pkgUrl pkg) ++ "\n" ++
                         "bugReports  = " ++ (show $ bugReports pkg) ++ "\n" ++
                         "synopsis    = " ++ (show $ synopsis pkg) ++ "\n" ++
                         "description = " ++ (show $ description pkg) ++ "\n"
