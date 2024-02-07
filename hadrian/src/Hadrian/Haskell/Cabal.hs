-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- Cabal files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (
    pkgPackageName, pkgVersion, pkgIdentifier, pkgSynopsis, pkgDescription, pkgDependencies,
    pkgGenericDescription, cabalArchString, cabalOsString,
    ) where

import Development.Shake
import Distribution.PackageDescription (GenericPackageDescription, unPackageName, PackageDescription (package))
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal

import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.Cabal
import Hadrian.Package

-- | The name of the package as written in the package's cabal file.
pkgPackageName :: Package -> Action String
pkgPackageName =
    fmap (unPackageName . Cabal.pkgName . package . Cabal.packageDescription) . pkgGenericDescription

-- | Read a Cabal file and return the package version. The Cabal file is tracked.
pkgVersion :: Package -> Action String
pkgVersion = fmap version . readPackageData

-- | Read a Cabal file and return the package identifier, e.g. @base-4.10.0.0@.
-- The Cabal file is tracked.
pkgIdentifier :: Package -> Action String
pkgIdentifier package = do
    cabal <- readPackageData package
    return $ if null (version cabal)
        then name cabal
        else name cabal ++ "-" ++ version cabal

-- | Read a Cabal file and return the package synopsis. The Cabal file is tracked.
pkgSynopsis :: Package -> Action String
pkgSynopsis = fmap synopsis . readPackageData

-- | Read a Cabal file and return the package description. The Cabal file is
-- tracked.
pkgDescription :: Package -> Action String
pkgDescription = fmap description . readPackageData

-- | Read a Cabal file and return the sorted list of the package dependencies.
-- The current version does not take care of Cabal conditionals and therefore
-- returns a crude overapproximation of actual dependencies. The Cabal file is
-- tracked.
pkgDependencies :: Package -> Action [PackageName]
pkgDependencies =
    fmap (map Hadrian.Package.pkgName . packageDependencies) . readPackageData

-- | Read a Cabal file and return the 'GenericPackageDescription'. The Cabal
-- file is tracked.
pkgGenericDescription :: Package -> Action GenericPackageDescription
pkgGenericDescription = fmap genericPackageDescription . readPackageData

-- | Cabal's rendering of an architecture as used in its directory structure.
--
-- Inverse of 'Cabal.Distribution.Simple.GHC.ghcArchString'.
cabalArchString :: String -> String
cabalArchString "powerpc"     = "ppc"
cabalArchString "powerpc64"   = "ppc64"
cabalArchString "powerpc64le" = "ppc64"
cabalArchString other         = other

-- | Cabal's rendering of an OS as used in its directory structure.
--
-- Inverse of 'Cabal.Distribution.Simple.GHC.ghcOsString'.
cabalOsString :: String -> String
cabalOsString "mingw32"  = "windows"
cabalOsString "darwin"   = "osx"
cabalOsString "solaris2" = "solaris"
cabalOsString other      = other
