%
% (c) The University of Glasgow, 2000
%
\section[CmStaticInfo]{Session-static info for the Compilation Manager}

\begin{code}
module CmStaticInfo ( FLAGS, Package(..), PCI, 
                      mkSI, SI(..)
                    )
where

#include "HsVersions.h"

\end{code}

\begin{code}
type FLAGS = [String]       -- or some such fiction
type PCI = [Package]

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
  deriving (Read, Show)


data SI = SI { flags :: FLAGS, pci :: PCI }

mkSI :: FLAGS -> PCI -> SI
mkSI flags pci = SI { flags = flags, pci = pci }


\end{code}
