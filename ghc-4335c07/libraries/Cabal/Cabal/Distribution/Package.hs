{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Defines a package identifier along with a parser and pretty printer for it.
-- 'PackageIdentifier's consist of a name and an exact version. It also defines
-- a 'Dependency' data type. A dependency is a package name and a version
-- range, like @\"foo >= 1.2 && < 2\"@.

module Distribution.Package
  ( module Distribution.Types.AbiHash
  , module Distribution.Types.ComponentId
  , module Distribution.Types.PackageId
  , module Distribution.Types.UnitId
  , module Distribution.Types.Module
  , module Distribution.Types.PackageName
  , module Distribution.Types.PkgconfigName
  , module Distribution.Types.Dependency
  , Package(..), packageName, packageVersion
  , HasMungedPackageId(..), mungedName', mungedVersion'
  , HasUnitId(..)
  , installedPackageId
  , PackageInstalled(..)
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
         ( Version )

import Distribution.Types.AbiHash
import Distribution.Types.ComponentId
import Distribution.Types.Dependency
import Distribution.Types.MungedPackageId
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Types.Module
import Distribution.Types.MungedPackageName
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigName

-- | Class of things that have a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
-- Not all kinds of packages can be uniquely identified by a
-- 'PackageIdentifier'. In particular, installed packages cannot, there may be
-- many installed instances of the same source package.
--
class Package pkg where
  packageId  :: pkg -> PackageIdentifier

mungedName'    :: HasMungedPackageId pkg => pkg -> MungedPackageName
mungedName'     = mungedName    . mungedId

mungedVersion' :: HasMungedPackageId munged => munged -> Version
mungedVersion'  = mungedVersion . mungedId

class HasMungedPackageId pkg where
  mungedId :: pkg -> MungedPackageId

instance Package PackageIdentifier where
  packageId = id

packageName    :: Package pkg => pkg -> PackageName
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId

instance HasMungedPackageId MungedPackageId where
  mungedId = id

-- | Packages that have an installed unit ID
class Package pkg => HasUnitId pkg where
  installedUnitId :: pkg -> UnitId

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Compatibility wrapper for Cabal pre-1.24.
installedPackageId :: HasUnitId pkg => pkg -> UnitId
installedPackageId = installedUnitId

-- | Class of installed packages.
--
-- The primary data type which is an instance of this package is
-- 'InstalledPackageInfo', but when we are doing install plans in Cabal install
-- we may have other, installed package-like things which contain more metadata.
-- Installed packages have exact dependencies 'installedDepends'.
class (HasUnitId pkg) => PackageInstalled pkg where
  installedDepends :: pkg -> [UnitId]
