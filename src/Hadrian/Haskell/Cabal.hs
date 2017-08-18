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
module Hadrian.Haskell.Cabal (readCabal, pkgNameVersion, pkgDependencies) where

import Development.Shake
import qualified Distribution.Package                  as C
import qualified Distribution.PackageDescription       as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Text                     as C
import qualified Distribution.Types.CondTree           as C
import qualified Distribution.Verbosity                as C

import Hadrian.Haskell.Package

-- | Read the @.cabal@ file of a given package and return the
-- 'GenericPackageDescription'. The @.cabal@ file is tracked.
readCabal :: Package -> Action C.GenericPackageDescription
readCabal pkg = do
    need [pkgCabalFile pkg]
    liftIO $ C.readGenericPackageDescription C.silent (pkgCabalFile pkg)

-- | Read the @.cabal@ file of a given package and return the package name and
-- version. The @.cabal@ file is tracked.
pkgNameVersion :: Package -> Action (PackageName, String)
pkgNameVersion pkg = do
    pkgId <- C.package . C.packageDescription <$> readCabal pkg
    return (C.unPackageName $ C.pkgName pkgId, C.display $ C.pkgVersion pkgId)

-- | Read the @.cabal@ file of a given package and return the list of its
-- dependencies. The current version does not take care of Cabal conditionals
-- and therefore returns a crude overapproximation of The @.cabal@ file is tracked.
pkgDependencies :: Package -> Action [PackageName]
pkgDependencies pkg = do
    gpd <- readCabal pkg
    let libDeps = collectDeps (C.condLibrary gpd)
        exeDeps = map (collectDeps . Just . snd) (C.condExecutables gpd)
    return [ C.unPackageName p | C.Dependency p _ <- concat (libDeps : exeDeps) ]

collectDeps :: Maybe (C.CondTree v [C.Dependency] a) -> [C.Dependency]
collectDeps Nothing = []
collectDeps (Just (C.CondNode _ deps ifs)) = deps ++ concatMap f ifs
  where
    f (C.CondBranch _ t mt) = collectDeps (Just t) ++ collectDeps mt
