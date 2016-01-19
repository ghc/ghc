module Rules.Cabal (cabalRules) where

import Base
import Data.Version
import Distribution.Package as DP
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Expression
import GHC
import Rules.Actions
import Settings

cabalRules :: Rules ()
cabalRules = do
    -- Cache boot package constraints (to be used in cabalArgs)
    bootPackageConstraints %> \out -> do
        bootPkgs <- interpretWithStage Stage0 getPackages
        let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
        constraints <- forM (sort pkgs) $ \pkg -> do
            need [pkgCabalFile pkg]
            pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
            let identifier          = package . packageDescription $ pd
                version             = showVersion . pkgVersion $ identifier
                DP.PackageName name = DP.pkgName identifier
            return $ name ++ " == " ++ version
        writeFileChanged out . unlines $ constraints

    -- Cache package dependencies
    packageDependencies %> \out -> do
        pkgs <- interpretWithStage Stage1 getPackages
        pkgDeps <- forM (sort pkgs) $ \pkg ->
            if pkg == rts
            then return $ pkgNameString pkg
            else do
                need [pkgCabalFile pkg]
                pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
                let depsLib  = collectDeps $ condLibrary pd
                    depsExes = map (collectDeps . Just . snd) $ condExecutables pd
                    deps     = concat $ depsLib : depsExes
                    depNames = [ name | Dependency (DP.PackageName name) _ <- deps ]
                return . unwords $ pkgNameString pkg : sort depNames
        writeFileChanged out . unlines $ pkgDeps

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (_, t, mt) = collectDeps (Just t) ++ collectDeps mt
