%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Calling conventions]{External calling conventions}

\begin{code}
module CallConv
       (
	 CallConv
       , pprCallConv
       , callConvToInt

       , stdCallConv
       , cCallConv
       , defaultCallConv
       , callConvAttribute
       ) where

#include "HsVersions.h"

import Outputable
import PrimRep     ( PrimRep, getPrimRepSizeInBytes )
\end{code}

\begin{code}
type CallConv = Int

pprCallConv :: CallConv -> SDoc
pprCallConv 0 = ptext SLIT("__stdcall")
pprCallConv _ = ptext SLIT("_ccall")

stdCallConv :: CallConv
stdCallConv = 0

cCallConv  :: CallConv
cCallConv = 1

defaultCallConv :: CallConv
defaultCallConv = cCallConv

callConvToInt :: CallConv -> Int
callConvToInt x = x
\end{code}

Generate the gcc attribute corresponding to the given
calling convention (used by PprAbsC):

ToDo: The stdcall calling convention is x86 (win32) specific,
so perhaps we should emit a warning if it's being used on other
platforms.

\begin{code}
callConvAttribute :: CallConv -> String
callConvAttribute cc
 | cc == stdCallConv   = "__stdcall"
 | cc == cCallConv     = ""
 | otherwise	       = panic ("callConvAttribute: cannot handle" ++ showSDoc (pprCallConv cc))

\end{code}
