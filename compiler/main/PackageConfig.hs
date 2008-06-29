--
-- (c) The University of Glasgow, 2004
--

module PackageConfig (
	-- * PackageId
	mkPackageId, packageConfigId, unpackPackageId,
	
	-- * The PackageConfig type: information about a package
	PackageConfig,
	InstalledPackageInfo_(..), display,
	Version(..),
	PackageIdentifier(..),
	defaultPackageConfig,
    packageConfigToInstalledPackageInfo,
    installedPackageInfoToPackageConfig,
  ) where

#include "HsVersions.h"

import Data.Maybe
import Module
import Distribution.InstalledPackageInfo
import Distribution.ModuleName
import Distribution.Package
import Distribution.Text
import Distribution.Version
import Distribution.Compat.ReadP

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is just InstalledPackageInfo from Cabal.  Later we
-- might need to extend it with some GHC-specific stuff, but for now it's fine.

type PackageConfig = InstalledPackageInfo_ Module.ModuleName
defaultPackageConfig :: PackageConfig
defaultPackageConfig = emptyInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- PackageId (package names with versions)

-- Mostly the compiler deals in terms of PackageNames, which don't
-- have the version suffix.  This is so that we don't need to know the
-- version for the -package-name flag, or know the versions of
-- wired-in packages like base & rts.  Versions are confined to the
-- package sub-system.
--
-- This means that in theory you could have multiple base packages installed
-- (for example), and switch between them using -package/-hide-package.
--
-- A PackageId is a string of the form <pkg>-<version>.

mkPackageId :: PackageIdentifier -> PackageId
mkPackageId = stringToPackageId . display

packageConfigId :: PackageConfig -> PackageId
packageConfigId = mkPackageId . package

unpackPackageId :: PackageId -> Maybe PackageIdentifier
unpackPackageId p
  = case [ pid | (pid,"") <- readP_to_S parse str ] of
        []      -> Nothing
        (pid:_) -> Just pid
  where str = packageIdString p

packageConfigToInstalledPackageInfo :: PackageConfig -> InstalledPackageInfo
packageConfigToInstalledPackageInfo
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 hiddenModules  = map convert h }
    where convert :: Module.ModuleName -> Distribution.ModuleName.ModuleName
          convert = fromJust . simpleParse . moduleNameString

installedPackageInfoToPackageConfig :: InstalledPackageInfo -> PackageConfig
installedPackageInfoToPackageConfig
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 hiddenModules  = map convert h }
    where convert :: Distribution.ModuleName.ModuleName -> Module.ModuleName
          convert = mkModuleName . display
