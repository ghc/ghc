%
% (c) The University of Glasgow, 2000
%
\section{Package manipulation}

\begin{code}
module Packages (
	PackageConfig(..), 
	defaultPackageConfig,
	packageDependents, 
	showPackages,

	PackageName,		-- Instance of Outputable
	mkPackageName, packageNameString,
	basePackage, rtsPackage, haskell98Package, thPackage, -- :: PackageName

	PackageConfigMap, emptyPkgMap, lookupPkg,
	extendPackageConfigMap, getPackageDetails, getPackageConfigMap,
    )
where

#include "HsVersions.h"

import CmdLineOpts	( dynFlag, verbosity )
import ErrUtils		( dumpIfSet )
import Outputable	( docToSDoc )
import FastString
import UniqFM
import Util
import Pretty

import DATA_IOREF

-- -----------------------------------------------------------------------------
-- The PackageConfig type

#define WANT_PRETTY
#define INTERNAL_PRETTY
-- Yes, do generate pretty-printing stuff for packages, and use our
-- own Pretty library rather than Text.PrettyPrint

-- There's a blob of code shared with ghc-pkg, 
-- so we just include it from there 
-- Primarily it defines	PackageConfig (a record)

#include "../utils/ghc-pkg/Package.hs"

-- -----------------------------------------------------------------------------
-- Package names

type PackageName = FastString	-- No encoding at all

mkPackageName :: String -> PackageName
mkPackageName = mkFastString

packageNameString :: PackageName -> String
packageNameString = unpackFS

rtsPackage, basePackage, haskell98Package, thPackage :: PackageName
basePackage      = FSLIT("base")
rtsPackage 	 = FSLIT("rts")
haskell98Package = FSLIT("haskell98")
thPackage        = FSLIT("haskell-src")	-- Template Haskell libraries in here

packageDependents :: PackageConfig -> [PackageName]
-- Impedence matcher, because PackageConfig has Strings 
-- not PackageNames at the moment.  Sigh.
packageDependents pkg = map mkPackageName (package_deps pkg)

-- -----------------------------------------------------------------------------
-- A PackageConfigMap maps a PackageName to a PackageConfig

type PackageConfigMap = UniqFM PackageConfig

lookupPkg    :: PackageConfigMap -> PackageName -> Maybe PackageConfig

emptyPkgMap  :: PackageConfigMap

emptyPkgMap  = emptyUFM
lookupPkg    = lookupUFM

extendPkgMap :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPkgMap pkg_map new_pkgs 
  = foldl add pkg_map new_pkgs
  where
    add pkg_map p = addToUFM pkg_map (mkFastString (name p)) p

GLOBAL_VAR(v_Package_details, emptyPkgMap, PackageConfigMap)

getPackageConfigMap :: IO PackageConfigMap
getPackageConfigMap = readIORef v_Package_details

extendPackageConfigMap :: [PackageConfig] -> IO ()
extendPackageConfigMap pkg_configs = do
  old_pkg_map <- readIORef v_Package_details
  writeIORef v_Package_details (extendPkgMap old_pkg_map pkg_configs)

getPackageDetails :: [PackageName] -> IO [PackageConfig]
getPackageDetails ps = do
  pkg_details <- getPackageConfigMap
  return [ pkg | Just pkg <- map (lookupPkg pkg_details) ps ]


-- -----------------------------------------------------------------------------
-- Displaying packages

showPackages :: PackageConfigMap -> IO ()
-- Show package info on console, if verbosity is >= 3
showPackages pkg_map
  = do  { verb <- dynFlag verbosity
	; dumpIfSet (verb >= 3) "Packages"
	  	    (docToSDoc (vcat (map dumpPkgGuts ps)))
	}
  where
    ps = eltsUFM pkg_map

\end{code}
