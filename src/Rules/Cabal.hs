module Rules.Cabal (cabalRules) where

import Expression
import Data.Version
import Distribution.Package hiding (Package)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Package hiding (library)
import Settings

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
            let depsLib  = collectDeps $ condLibrary pd
                depsExes = map (collectDeps . Just . snd) $ condExecutables pd
                deps     = concat $ depsLib : depsExes
                depNames = [ name | Dependency (PackageName name) _ <- deps ]
            return . unwords $ Package.pkgName pkg : sort depNames
        writeFileChanged out . unlines $ pkgDeps

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (_, t, mt) = collectDeps (Just t) ++ collectDeps mt
