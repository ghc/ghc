%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelMods]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod 

[oh dear, looks like the recursive module monster caught up with
 and gobbled whoever was writing the above :-) -- SOF ]

\begin{code}
module PrelMods
        (
         gHC__, pRELUDE, pREL_BASE,
         pREL_READ , pREL_NUM, pREL_LIST,
	 pREL_TUP  , pACKED_STRING, cONC_BASE,
         iO_BASE   , eRROR, mONAD, rATIO, iX,
         sT_BASE   , aRR_BASE, fOREIGN, mAIN,
         gHC_MAIN  , gHC_ERR,
	 cCALL     , aDDR
	) where

#include "HsVersions.h"

import BasicTypes( Module )
\end{code}

\begin{code}
gHC__, pRELUDE, pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP :: Module
pACKED_STRING, cONC_BASE, iO_BASE, mONAD, rATIO, iX      :: Module
sT_BASE, aRR_BASE, fOREIGN, mAIN, gHC_MAIN, gHC_ERR      :: Module	

gHC__	     = SLIT("GHC")	   -- Primitive types and values

pRELUDE	     = SLIT("Prelude")
pREL_BASE    = SLIT("PrelBase")
pREL_READ    = SLIT("PrelRead")
pREL_NUM     = SLIT("PrelNum")
pREL_LIST    = SLIT("PrelList")
pREL_TUP     = SLIT("PrelTup")
pACKED_STRING= SLIT("PackBase")
cONC_BASE    = SLIT("ConcBase")
iO_BASE	     = SLIT("IOBase")
eRROR	     = SLIT("Error")
mONAD	     = SLIT("Monad")
rATIO	     = SLIT("Ratio")
iX	     = SLIT("Ix")
sT_BASE	     = SLIT("STBase")
aRR_BASE     = SLIT("ArrBase")
fOREIGN	     = SLIT("Foreign")
cCALL        = SLIT("CCall")
aDDR         = SLIT("Addr")

mAIN	     = SLIT("Main")
gHC_MAIN     = SLIT("GHCmain")
gHC_ERR	     = SLIT("GHCerr")

\end{code}
