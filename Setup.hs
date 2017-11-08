-- Copyright 2017 Bose Corporation.
-- This software is released under the 3-Clause BSD License.
-- The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Package (packageVersion, packageName, PackageIdentifier(..), PackageName(..))
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..), Args)
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags(..), fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import System.FilePath ((</>), (<.>))
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Data.Version (showVersion)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

main = defaultMainWithHooks packageInfoUserHooks

packageInfoUserHooks :: UserHooks
packageInfoUserHooks =
    simpleUserHooks {
        buildHook = genPackageInfoHook
    }

app_name :: PackageIdentifier -> String
app_name packageInfo = ((\ (PackageName s) -> s) $ packageName packageInfo)

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
genPackageInfoHook pkg lbi uhs bfs= do
    createDirectoryIfMissingVerbose (fromFlag $ buildVerbosity bfs) True (autogenModulesDir lbi)
    let packageInfoModulePath = autogenModulesDir lbi </> cfg_name <.> "hs"
    rewriteFile packageInfoModulePath . generate =<< build_commit
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
