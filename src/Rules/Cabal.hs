module Rules.Cabal (cabalRules) where

import Base
import Stage
import Package hiding (library)
import Expression hiding (package)
import Settings.Packages
import Data.List
import Data.Version
import Distribution.Package
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

cabalRules :: Rules ()
cabalRules = do
    -- Cache boot package constraints (to be used in cabalArgs)
    bootPackageConstraints %> \file -> do
        pkgs <- interpret (stageTarget Stage0) getPackages
        constraints <- forM (sort pkgs) $ \pkg -> do
            let cabal = pkgCabalPath pkg
            need [cabal]
            description <- liftIO $ readPackageDescription silent cabal
            let identifier       = package . packageDescription $ description
                version          = showVersion . pkgVersion $ identifier
                PackageName name = Distribution.Package.pkgName identifier
            return $ name ++ " == " ++ version
        writeFileChanged file . unlines $ constraints

    -- Cache package dependencies
    packageDependencies %> \file -> do
        pkgs <- interpret (stageTarget Stage1) getPackages
        pkgDeps <- forM (sort pkgs) $ \pkg -> do
            let cabal = pkgCabalPath pkg
            need [cabal]
            description <- liftIO $ readPackageDescription silent cabal
            let deps     = collectDeps . condLibrary $ description
                depNames = [ name | Dependency (PackageName name) _ <- deps ]
            return . unwords $ Package.pkgName pkg : sort depNames
        writeFileChanged file . unlines $ pkgDeps

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (_, t, mt) = collectDeps (Just t) ++ collectDeps mt
