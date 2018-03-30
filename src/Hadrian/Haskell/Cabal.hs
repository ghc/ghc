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

import Data.Maybe
import Development.Shake

import Context.Type
import Hadrian.Haskell.Cabal.Type        as C
import Hadrian.Haskell.Cabal.PackageData as PD
import Hadrian.Package
import Hadrian.Oracles.TextFile

-- | Read a Cabal file and return the package version. The Cabal file is tracked.
pkgVersion :: Context -> Action (Maybe String)
pkgVersion = fmap (fmap C.version) . readCabalFile

-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
pkgIdentifier :: Context -> Action String
pkgIdentifier ctx = do
    cabal <- fromMaybe (error "Cabal file could not be read") <$> readCabalFile ctx
    return $ if null (C.version cabal)
        then C.name cabal
        else C.name cabal ++ "-" ++ C.version cabal

-- | Read a Cabal file and return the sorted list of the package dependencies.
-- The current version does not take care of Cabal conditionals and therefore
-- returns a crude overapproximation of actual dependencies. The Cabal file is
-- tracked.
pkgDependencies :: Context -> Action (Maybe [PackageName])
pkgDependencies = fmap (fmap PD.dependencies) . readPackageDataFile

-- | Read a Cabal file and return the package synopsis. The Cabal file is tracked.
pkgSynopsis :: Context -> Action (Maybe String)
pkgSynopsis = fmap (fmap C.synopsis) . readCabalFile
