%
% (c) The University of Glasgow, 2000
%
\section{Package manipulation}

\begin{code}
module Packages (
	PackageConfig(..), 
	defaultPackageConfig,
	mungePackagePaths, packageDependents, 
	showPackages,

	PackageName,		-- Instance of Outputable
	mkPackageName, packageNameString,
	basePackage, rtsPackage, haskell98Package, thPackage,	-- :: PackageName

	PackageConfigMap, emptyPkgMap, extendPkgMap, lookupPkg
    )
where

#include "HsVersions.h"

import Pretty

import CmdLineOpts	( dynFlag, verbosity )
import DriverUtil	( my_prefix_match )
import ErrUtils		( dumpIfSet )
import Outputable	( docToSDoc )
import FastString
import UniqFM
\end{code}

%*********************************************************
%*						 	 *
\subsection{Basic data types}
%*							 *
%*********************************************************

\begin{code}
#define WANT_PRETTY
#define INTERNAL_PRETTY
-- Yes, do generate pretty-printing stuff for packages, and use our
-- own Pretty library rather than Text.PrettyPrint

-- There's a blob of code shared with ghc-pkg, 
-- so we just include it from there 
-- Primarily it defines
--	PackageConfig (a record)
--	PackageName   (FastString)

#include "../utils/ghc-pkg/Package.hs"
\end{code}

\begin{code}
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
\end{code}

A PackageConfigMap maps a PackageName to a PackageConfig

\begin{code}
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
\end{code}

%*********************************************************
%*						 	 *
\subsection{Load the config file}
%*							 *
%*********************************************************

\begin{code}
mungePackagePaths :: String -> [PackageConfig] -> [PackageConfig]
-- Replace the string "$libdir" at the beginning of a path
-- with the current libdir (obtained from the -B option).
mungePackagePaths top_dir ps = map munge_pkg ps
 where 
  munge_pkg p = p{ import_dirs  = munge_paths (import_dirs p),
		   include_dirs = munge_paths (include_dirs p),
    		   library_dirs = munge_paths (library_dirs p),
		   framework_dirs = munge_paths (framework_dirs p) }

  munge_paths = map munge_path

  munge_path p 
	  | Just p' <- my_prefix_match "$libdir" p = top_dir ++ p'
	  | otherwise				   = p
\end{code}


%*********************************************************
%*						 	 *
\subsection{Display results}
%*							 *
%*********************************************************

\begin{code}
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
