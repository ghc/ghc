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
       , decorateExtName
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
 | cc == stdCallConv   = "__attribute__((stdcall))"
 | cc == cCallConv     = ""
 | otherwise	       = panic ("callConvAttribute: cannot handle" ++ showSDoc (pprCallConv cc))

\end{code}

For stdcall and Win32, the linker expects to see names of the form 
 
   "f@n"

where n is the size (in 8-bit bytes) of the parameter area
that is pushed onto the stack before invocation. We take
care of mangling the function name here. 

This name mangler is only used by the x86 native code generator.

\begin{code}
decorateExtName :: CallConv -> FAST_STRING -> [PrimRep] -> FAST_STRING
decorateExtName cc fs ps
 | cc /= stdCallConv = fs
 | otherwise	     = fs _APPEND_ (_PK_ ('@':show (size::Int)))
 where
  size = sum (map (adjustParamSize.getPrimRepSizeInBytes) ps)

  adjustParamSize sz =  paramBoundary * ((sz + paramBoundary - 1) `div` paramBoundary)

  paramBoundary = 4

\end{code}
