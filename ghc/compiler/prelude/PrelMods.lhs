%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelMods]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod
\begin{code}
#include "HsVersions.h"

module PrelMods (
	gHC_BUILTINS, -- things that are really and truly primitive
	pRELUDE, gHC__,
	rATIO, iX,
	modulesWithBuiltins
  ) where

CHK_Ubiq() -- debugging consistency check
\end{code}


\begin{code}
pRELUDE	     = SLIT("Prelude")
gHC_BUILTINS = SLIT("GHCbuiltins") -- the truly-primitive things
gHC__	     = SLIT("GHCbase")	   -- all GHC basics, add-ons, extras, everything
				   -- (which can be defined in Haskell)
rATIO	     = SLIT("Ratio")
iX	     = SLIT("Ix")

modulesWithBuiltins = [ gHC_BUILTINS, gHC__, pRELUDE, rATIO, iX ]
\end{code}
