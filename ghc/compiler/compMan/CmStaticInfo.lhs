%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( GhciMode(..), PackageConfig(..), defaultPackageConfig )
where

#include "HsVersions.h"

\end{code}

\begin{code}
data GhciMode = Batch | Interactive | OneShot 
     deriving Eq

data PackageConfig
   = PackageConfig {
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

defaultPackageConfig
   = PackageConfig {
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
