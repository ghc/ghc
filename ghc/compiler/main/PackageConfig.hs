--
-- (c) The University of Glasgow, 2004
--

module PackageConfig (
	-- * PackageId
	PackageId, 
	mkPackageId, stringToPackageId, packageIdString, packageConfigId,
	packageIdFS, fsToPackageId, 
	
	-- * The PackageConfig type: information about a package
	PackageConfig,
	InstalledPackageInfo(..), showPackageId,
	Version(..),
	PackageIdentifier(..),
	defaultPackageConfig
  ) where

#include "HsVersions.h"

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Version
import FastString

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is just InstalledPackageInfo from Cabal.  Later we
-- might need to extend it with some GHC-specific stuff, but for now it's fine.

type PackageConfig = InstalledPackageInfo
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

newtype PackageId = PId FastString deriving( Eq, Ord )  -- includes the version
	-- easier not to use a newtype here, because we need instances of
	-- Binary & Outputable, and we're too early to define them

fsToPackageId :: FastString -> PackageId
fsToPackageId = PId

packageIdFS :: PackageId -> FastString
packageIdFS (PId fs) = fs

stringToPackageId :: String -> PackageId
stringToPackageId = fsToPackageId . mkFastString

packageIdString :: PackageId -> String
packageIdString = unpackFS . packageIdFS

mkPackageId :: PackageIdentifier -> PackageId
mkPackageId = stringToPackageId . showPackageId

packageConfigId :: PackageConfig -> PackageId
packageConfigId = mkPackageId . package


