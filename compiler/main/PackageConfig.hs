--
-- (c) The University of Glasgow, 2004
--

module PackageConfig (
	-- * PackageId
	PackageId, 
	mkPackageId, stringToPackageId, packageIdString, packageConfigId,
	packageIdFS, fsToPackageId,  unpackPackageId,
	
	-- * The PackageConfig type: information about a package
	PackageConfig,
	InstalledPackageInfo(..), showPackageId,
	Version(..),
	PackageIdentifier(..),
	defaultPackageConfig,

	-- * Wired-in PackageIds
	basePackageId,
	rtsPackageId,
	haskell98PackageId,
	thPackageId,
	mainPackageId
  ) where

#include "HsVersions.h"

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Version
import FastString
import Text.ParserCombinators.ReadP ( readP_to_S )

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

unpackPackageId :: PackageId -> Maybe PackageIdentifier
unpackPackageId p
  = case [ pid | (pid,"") <- readP_to_S parsePackageId str ] of
        []      -> Nothing
        (pid:_) -> Just pid
  where str = packageIdString p

-- -----------------------------------------------------------------------------
-- Package Ids that are wired in

-- Certain packages are "known" to the compiler, in that we know about certain
-- entities that reside in these packages, and the compiler needs to 
-- declare static Modules and Names that refer to these packages.  Hence
-- the wired-in packages can't include version numbers, since we don't want
-- to bake the version numbers of these packages into GHC.
--
-- So here's the plan.  Wired-in packages are still versioned as
-- normal in the packages database, and you can still have multiple
-- versions of them installed.  However, for each invocation of GHC,
-- only a single instance of each wired-in package will be recognised
-- (the desired one is selected via -package/-hide-package), and GHC
-- will use the unversioned PackageId below when referring to it,
-- including in .hi files and object file symbols.  Unselected
-- versions of wired-in packages will be ignored, as will any other
-- package that depends directly or indirectly on it (much as if you
-- had used -ignore-package).

basePackageId      = fsToPackageId FSLIT("base")
rtsPackageId	   = fsToPackageId FSLIT("rts")
haskell98PackageId = fsToPackageId FSLIT("haskell98")
thPackageId        = fsToPackageId FSLIT("template-haskell")

-- This is the package Id for the program.  It is the default package
-- Id if you don't specify a package name.  We don't add this prefix
-- to symbol name, since there can be only one main package per program.
mainPackageId	   = fsToPackageId FSLIT("main")

