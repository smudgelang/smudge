import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(..), Args)
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags(..), fromFlag)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import System.FilePath ((</>), (<.>))

main = defaultMainWithHooks packageInfoUserHooks

packageInfoUserHooks :: UserHooks
packageInfoUserHooks =
    simpleUserHooks {
        buildHook = genPackageInfoHook
    }

genPackageInfoHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
genPackageInfoHook pkg lbi uhs bfs= do
    createDirectoryIfMissingVerbose (fromFlag $ buildVerbosity bfs) True (autogenModulesDir lbi)
    let packageInfoModulePath = autogenModulesDir lbi </> cfg_name <.> "hs"
    rewriteFile packageInfoModulePath (generate pkg)
    buildHook simpleUserHooks pkg lbi uhs bfs
    where cfg_name = "PackageInfo"
          generate pkg = "module " ++ cfg_name ++ " where\n" ++
                         "\n" ++
                         "import Distribution.Package (PackageIdentifier(..), PackageName(..))\n" ++
                         "import Distribution.Version (Version(..))\n" ++
                         "\n" ++
                         "packageInfo = " ++ (show $ package pkg) ++ "\n" ++
                         "copyright   = " ++ (show $ copyright pkg) ++ "\n" ++
                         "maintainer  = " ++ (show $ maintainer pkg) ++ "\n" ++
                         "author      = " ++ (show $ author pkg) ++ "\n" ++
                         "stability   = " ++ (show $ stability pkg) ++ "\n" ++
                         "homepage    = " ++ (show $ homepage pkg) ++ "\n" ++
                         "pkgUrl      = " ++ (show $ pkgUrl pkg) ++ "\n" ++
                         "bugReports  = " ++ (show $ bugReports pkg) ++ "\n" ++
                         "synopsis    = " ++ (show $ synopsis pkg) ++ "\n" ++
                         "description = " ++ (show $ description pkg) ++ "\n"
