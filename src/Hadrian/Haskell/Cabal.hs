-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- @.cabal@ files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (readCabal, cabalNameVersion, cabalDependencies) where

import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Types.CondTree
import Distribution.Verbosity

-- | Read a given @.cabal@ file and return the 'GenericPackageDescription'. The
-- @.cabal@ file is tracked.
readCabal :: FilePath -> Action GenericPackageDescription
readCabal cabal = do
    need [cabal]
    liftIO $ readGenericPackageDescription silent cabal

-- | Read a given @.cabal@ file and return the package name and version. The
-- @.cabal@ file is tracked.
cabalNameVersion :: FilePath -> Action (String, String)
cabalNameVersion cabal = do
    identifier <- package . packageDescription <$> readCabal cabal
    return (unPackageName $ pkgName identifier, display $ pkgVersion identifier)

-- | Read a given @.cabal@ file and return the package dependencies. The
-- @.cabal@ file is tracked.
cabalDependencies :: FilePath -> Action [String]
cabalDependencies cabal = do
    gpd <- readCabal cabal
    let libDeps = collectDeps (condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (condExecutables gpd)
    return [ unPackageName p | Dependency p _ <- concat (libDeps : exeDeps) ]

collectDeps :: Maybe (CondTree v [Dependency] a) -> [Dependency]
collectDeps Nothing = []
collectDeps (Just (CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
