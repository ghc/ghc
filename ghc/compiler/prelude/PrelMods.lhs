%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelMods]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod
\begin{code}
#include "HsVersions.h"

module PrelMods where

CHK_Ubiq() -- debugging consistency check
\end{code}


\begin{code}
gHC__	     = SLIT("GHC")	   -- Primitive types and values

pRELUDE	     = SLIT("Prelude")
pREL_BASE    = SLIT("PrelBase")
pREL_READ    = SLIT("PrelRead")
pREL_NUM     = SLIT("PrelNum")
pREL_LIST    = SLIT("PrelList")
pREL_TUP     = SLIT("PrelTup")
pACKED_STRING= SLIT("PackedString")
cONC_BASE    = SLIT("ConcBase")
iO_BASE	     = SLIT("IOBase")
mONAD	     = SLIT("Monad")
rATIO	     = SLIT("Ratio")
iX	     = SLIT("Ix")
sT_BASE	     = SLIT("STBase")
aRR_BASE     = SLIT("ArrBase")
fOREIGN	     = SLIT("Foreign")

mAIN	     = SLIT("Main")
gHC_MAIN     = SLIT("GHCmain")
gHC_ERR	     = SLIT("GHCerr")
\end{code}
