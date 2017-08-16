module Rules.Cabal (cabalRules) where

import Hadrian.Haskell.Cabal

import Base
import GHC
import Settings

cabalRules :: Rules ()
cabalRules = do
    -- Cache boot package constraints (to be used in 'cabalArgs').
    "//" -/- bootPackageConstraints %> \out -> do
        bootPkgs <- stagePackages Stage0
        let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
        constraints <- forM (sort pkgs) $ \pkg -> do
            (name, version) <- cabalNameVersion (pkgCabalFile pkg)
            return $ name ++ " == " ++ version
        writeFileChanged out . unlines $ constraints
        putSuccess $ "| Successfully generated boot package constraints"

    -- Cache package dependencies.
    "//" -/- packageDependencies %> \out -> do
        pkgDeps <- forM (sort knownPackages) $ \pkg -> do
            exists <- doesFileExist (pkgCabalFile pkg)
            if not exists then return $ pkgNameString pkg
            else do
                deps <- sort <$> cabalDependencies (pkgCabalFile pkg)
                return . unwords $ pkgNameString pkg : (deps \\ [pkgNameString pkg])
        writeFileChanged out $ unlines pkgDeps
        putSuccess $ "| Successfully generated package dependencies"
