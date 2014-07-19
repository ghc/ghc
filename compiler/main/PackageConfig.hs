{-# LANGUAGE CPP #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module PackageConfig (
        -- $package_naming

        -- * PackageId
        mkPackageId, packageConfigId,

        -- * The PackageConfig type: information about a package
        PackageConfig,
        InstalledPackageInfo_(..), display,
        Version(..),
        PackageIdentifier(..),
        defaultPackageConfig,
        packageConfigToInstalledPackageInfo,
        installedPackageInfoToPackageConfig
    ) where

#include "HsVersions.h"

import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Package hiding (PackageId)
import Distribution.Text
import Distribution.Version

import Maybes
import Module

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is just InstalledPackageInfo from Cabal. Later we
-- might need to extend it with some GHC-specific stuff, but for now it's fine.

type PackageConfig = InstalledPackageInfo_ Module.ModuleName

defaultPackageConfig :: PackageConfig
defaultPackageConfig = emptyInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- PackageId (package names with versions)

-- $package_naming
-- #package_naming#
-- Mostly the compiler deals in terms of 'PackageId's, which have the
-- form @<pkg>-<version>@. You're expected to pass in the version for
-- the @-package-name@ flag. However, for wired-in packages like @base@
-- & @rts@, we don't necessarily know what the version is, so these are
-- handled specially; see #wired_in_packages#.

-- | Turn a Cabal 'PackageIdentifier' into a GHC 'PackageId'
mkPackageId :: PackageIdentifier -> PackageId
mkPackageId = stringToPackageId . display

-- | Get the GHC 'PackageId' right out of a Cabalish 'PackageConfig'
packageConfigId :: PackageConfig -> PackageId
packageConfigId = mkPackageId . sourcePackageId

-- | Turn a 'PackageConfig', which contains GHC 'Module.ModuleName's into a Cabal specific
-- 'InstalledPackageInfo' which contains Cabal 'Distribution.ModuleName.ModuleName's
packageConfigToInstalledPackageInfo :: PackageConfig -> InstalledPackageInfo
packageConfigToInstalledPackageInfo
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 hiddenModules  = map convert h }
    where convert :: Module.ModuleName -> Distribution.ModuleName.ModuleName
          convert = (expectJust "packageConfigToInstalledPackageInfo") . simpleParse . moduleNameString

-- | Turn an 'InstalledPackageInfo', which contains Cabal 'Distribution.ModuleName.ModuleName's
-- into a GHC specific 'PackageConfig' which contains GHC 'Module.ModuleName's
installedPackageInfoToPackageConfig :: InstalledPackageInfo_ String -> PackageConfig
installedPackageInfoToPackageConfig
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map mkModuleName e,
                 hiddenModules  = map mkModuleName h }

