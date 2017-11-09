-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (
    pkgVersion, pkgIdentifier, pkgDependencies, pkgSynopsis
    ) where

import Development.Shake

import Hadrian.Haskell.Cabal.Parse
import Hadrian.Package
import Hadrian.Oracles.TextFile

-- | Read a Cabal file and return the package version. The Cabal file is tracked.
pkgVersion :: FilePath -> Action String
pkgVersion cabalFile = version <$> readCabalFile cabalFile

-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
pkgIdentifier :: FilePath -> Action String
pkgIdentifier cabalFile = do
    cabal <- readCabalFile cabalFile
    return $ if null (version cabal)
        then name cabal
        else name cabal ++ "-" ++ version cabal

-- | Read a Cabal file and return the sorted list of the package dependencies.
-- The current version does not take care of Cabal conditionals and therefore
-- returns a crude overapproximation of actual dependencies. The Cabal file is
-- tracked.
pkgDependencies :: FilePath -> Action [PackageName]
pkgDependencies cabalFile = dependencies <$> readCabalFile cabalFile

-- | Read a Cabal file and return the package synopsis. The Cabal file is tracked.
pkgSynopsis :: FilePath -> Action String
pkgSynopsis cabalFile = synopsis <$> readCabalFile cabalFile
