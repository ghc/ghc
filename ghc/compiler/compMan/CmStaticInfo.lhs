%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( GhciMode(..), Package(..), PackageConfigInfo, defaultPackage )
where

#include "HsVersions.h"

\end{code}

\begin{code}
data GhciMode = Batch | Interactive | OneShot 
     deriving Eq

type PackageConfigInfo = [Package]

data Package
   = Package {
	name            :: String,
	import_dirs     :: [String],
	source_dirs     :: [String],
	library_dirs    :: [String],
	hs_libraries    :: [String],
	extra_libraries :: [String],
	include_dirs    :: [String],
	c_includes      :: [String],
	package_deps    :: [String],
	extra_ghc_opts  :: [String],
	extra_cc_opts   :: [String],
	extra_ld_opts   :: [String]
     }

defaultPackage
   = Package {
	name = error "defaultPackage",
	import_dirs     = [],
	source_dirs     = [],
	library_dirs    = [],
	hs_libraries    = [],
	extra_libraries = [],
	include_dirs    = [],
	c_includes      = [],
	package_deps    = [],
	extra_ghc_opts  = [],
	extra_cc_opts   = [],
	extra_ld_opts   = []
    }
\end{code}
