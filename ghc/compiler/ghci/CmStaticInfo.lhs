%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( Package(..), PackageConfigInfo(..), mkPCI )
where

#include "HsVersions.h"

import Monad
\end{code}

\begin{code}
newtype PackageConfigInfo = PackageConfigInfo [Package]

-- copied from the driver
data Package
   = Package {
        name            :: String,
        import_dirs     :: [String],
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
  deriving Read

mkPCI :: [Package] -> IO PackageConfigInfo
mkPCI = return . PackageConfigInfo
\end{code}
