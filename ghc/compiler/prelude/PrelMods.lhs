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
	pREL_GHC, pRELUDE, mONAD, rATIO, iX, mAIN, pREL_MAIN, pREL_ERR,
	pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP, pREL_ADDR, pREL_READ,
	pREL_PACK, pREL_CONC, pREL_IO_BASE, pREL_ST, pREL_ARR, pREL_FOREIGN
	) where

#include "HsVersions.h"

import BasicTypes( Module )
\end{code}

\begin{code}
pREL_GHC, pRELUDE, mONAD, rATIO, iX, mAIN, pREL_MAIN, pREL_ERR      :: Module
pREL_BASE, pREL_NUM, pREL_LIST, pREL_TUP, pREL_ADDR, pREL_READ      :: Module	
pREL_PACK, pREL_CONC, pREL_IO_BASE, pREL_ST, pREL_ARR, pREL_FOREIGN :: Module	


pRELUDE	     = SLIT("Prelude")
pREL_GHC     = SLIT("PrelGHC")	   -- Primitive types and values
pREL_BASE    = SLIT("PrelBase")
pREL_READ    = SLIT("PrelRead")
pREL_NUM     = SLIT("PrelNum")
pREL_LIST    = SLIT("PrelList")
pREL_TUP     = SLIT("PrelTup")
pREL_PACK    = SLIT("PrelPack")
pREL_CONC    = SLIT("PrelConc")
pREL_IO_BASE = SLIT("PrelIOBase")
pREL_ST	     = SLIT("PrelST")
pREL_ARR     = SLIT("PrelArr")
pREL_FOREIGN = SLIT("PrelForeign")
pREL_CCALL   = SLIT("PrelCCall")
pREL_ADDR    = SLIT("PrelAddr")
pREL_ERR     = SLIT("PrelErr")

mONAD	     = SLIT("Monad")
rATIO	     = SLIT("Ratio")
iX	     = SLIT("Ix")

pREL_MAIN    = SLIT("PrelMain")
mAIN	     = SLIT("Main")

\end{code}
