module Rules.Cabal (cabalRules) where

import Base
import Stage
import Package hiding (library)
import Expression
import Settings.Packages
import Data.Version
import Distribution.Package
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

cabalRules :: Rules ()
cabalRules = do
    -- Cache boot package constraints (to be used in cabalArgs)
    bootPackageConstraints %> \out -> do
        pkgs <- interpretWithStage Stage0 getPackages
        constraints <- forM (sort pkgs) $ \pkg -> do
            need [pkgCabalFile pkg]
            pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
            let identifier       = package . packageDescription $ pd
                version          = showVersion . pkgVersion $ identifier
                PackageName name = Distribution.Package.pkgName identifier
            return $ name ++ " == " ++ version
        writeFileChanged out . unlines $ constraints

    -- Cache package dependencies
    packageDependencies %> \out -> do
        pkgs <- interpretWithStage Stage1 getPackages
        pkgDeps <- forM (sort pkgs) $ \pkg -> do
            need [pkgCabalFile pkg]
            pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
            let deps     = collectDeps . condLibrary $ pd
                depNames = [ name | Dependency (PackageName name) _ <- deps ]
            return . unwords $ Package.pkgName pkg : sort depNames
        writeFileChanged out . unlines $ pkgDeps

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (_, t, mt) = collectDeps (Just t) ++ collectDeps mt
