%
% (c) The University of Glasgow, 2000
%
\section{Package manipulation}

\begin{code}
module Packages ( PackageConfig(..), 
		  defaultPackageConfig,
		  mungePackagePaths,
		  showPackages
		)
where

#include "HsVersions.h"
import Pretty

import CmdLineOpts	( dynFlag, verbosity )
import DriverUtil	( my_prefix_match )
import ErrUtils		( dumpIfSet )
import Outputable	( docToSDoc )
\end{code}

\begin{code}
#define WANT_PRETTY
-- Yes, do generate pretty-printing stuff for packages

-- There's a blob of code shared with ghc-pkg, 
-- so we just include it from there 
#include "../utils/ghc-pkg/Package.hs"
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
    		   library_dirs = munge_paths (library_dirs p) }

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
showPackages :: [PackageConfig] -> IO ()
-- Show package info on console, if verbosity is >= 3
showPackages ps
  = do  { verb <- dynFlag verbosity
	; dumpIfSet (verb >= 3) "Packages"
	  	    (docToSDoc (vcat (map dumpPkgGuts ps)))
	}
\end{code}
