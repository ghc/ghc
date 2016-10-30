module Rules.Cabal (cabalRules) where

import Distribution.Package as DP
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity

import Base
import Expression
import GHC
import Settings
import Settings.Path

cabalRules :: Rules ()
cabalRules = do
    -- Cache boot package constraints (to be used in 'cabalArgs').
    bootPackageConstraints %> \out -> do
        bootPkgs <- stagePackages Stage0
        let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
        constraints <- forM (sort pkgs) $ \pkg -> do
            need [pkgCabalFile pkg]
            pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
            let identifier          = package . packageDescription $ pd
                version             = display . pkgVersion $ identifier
            return $ unPackageName (DP.pkgName identifier) ++ " == " ++ version
        writeFileChanged out . unlines $ constraints

    -- Cache package dependencies.
    packageDependencies %> \out -> do
        pkgs <- concatMapM stagePackages [Stage0 .. Stage2]
        pkgDeps <- forM (sort pkgs) $ \pkg ->
            if pkg `elem` [hp2ps, libffi, rts, touchy, unlit]
            then return $ pkgNameString pkg
            else do
                need [pkgCabalFile pkg]
                pd <- liftIO . readPackageDescription silent $ pkgCabalFile pkg
                let depsLib  = collectDeps $ condLibrary pd
                    depsExes = map (collectDeps . Just . snd) $ condExecutables pd
                    deps     = concat $ depsLib : depsExes
                    depNames = [ unPackageName name | Dependency name _ <- deps ]
                return . unwords $ pkgNameString pkg : sort depNames
        writeFileChanged out . unlines $ pkgDeps

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (_, t, mt) = collectDeps (Just t) ++ collectDeps mt
