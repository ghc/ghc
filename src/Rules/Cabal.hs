module Rules.Cabal (cabalRules) where

import Base
import Util
import Stage
import Package
import Expression
import Settings.Packages
import Data.List
import Data.Version
import qualified Distribution.Package                  as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.Verbosity                as D
import qualified Distribution.PackageDescription.Parse as D

cabalRules :: Rules ()
cabalRules =
    -- Cache boot package constraints (to be used in cabalArgs)
    bootPackageConstraints %> \file -> do
        pkgs <- interpret (stageTarget Stage0) packages
        constraints <- forM (sort pkgs) $ \pkg -> do
            let cabal = pkgPath pkg -/- pkgCabal pkg
            need [cabal]
            descr <- liftIO $ D.readPackageDescription D.silent cabal
            let identifier         = D.package . D.packageDescription $ descr
                version            = showVersion . D.pkgVersion $ identifier
                D.PackageName name = D.pkgName $ identifier
            return $ name ++ " == " ++ version
        writeFileChanged file . unlines $ constraints
