
module Main (main) where

import Control.Monad
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Utils
import System.Cmd
import System.Environment
import System.Exit

main :: IO ()
main = do let verbosity = 0
          mustBeBuildables <- getMustBeBuildablePackages
          dfd <- defaultPackageDesc verbosity
          pkgDescr <- readPackageDescription verbosity dfd
          mInfolFile <- defaultHookedPackageDesc
          info <- case mInfolFile of
                      Nothing -> return emptyHookedBuildInfo
                      Just infoFile -> readHookedBuildInfo verbosity infoFile
          let pkgDescr' = updatePackageDescription info pkgDescr
              pkg = pkgName (package pkgDescr')
              mustBeBuildable = pkg `elem` mustBeBuildables
              buildInfos = map libBuildInfo (maybeToList (library pkgDescr'))
                        ++ map buildInfo (executables pkgDescr')
              isBuildable = any buildable buildInfos
          when (mustBeBuildable || isBuildable) $ do
              args <- getArgs
              case args of
                  prog : progArgs ->
                      do ec <- rawSystem prog progArgs
                         exitWith ec
                  [] ->
                      error "ifBuildable: No command given"

getMustBeBuildablePackages :: IO [String]
getMustBeBuildablePackages
 = do xs <- readFile "../core-packages"
      return $ filter ("readline" /=) $ lines xs

