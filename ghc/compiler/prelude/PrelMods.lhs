%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelMods]{Definitions of prelude modules}

The strings identify built-in prelude modules.  They are
defined here so as to avod 

[oh dear, look like the recursive module monster caught up and
 gobbled whoever was writing the above :-) -- SOF ]

\begin{code}
#include "HsVersions.h"

module PrelMods
        (
	 isPreludeModule,   -- :: Module -> Bool

         gHC__, pRELUDE, pREL_BASE,
         pREL_READ , pREL_NUM, pREL_LIST,
	 pREL_TUP  , pACKED_STRING, cONC_BASE,
         iO_BASE   , mONAD, rATIO, iX,
         sT_BASE   , aRR_BASE, fOREIGN, mAIN,
         gHC_MAIN  , gHC_ERR
	) where

CHK_Ubiq() -- debugging consistency check
import UniqSet ( UniqSet(..), mkUniqSet, elementOfUniqSet )

\end{code}

Predicate used by RnIface to decide whether or not to
append a special suffix for prelude modules:

\begin{code}
isPreludeModule :: Module -> Bool
isPreludeModule mod = mod `elementOfUniqSet` preludeNames

preludeNames :: UniqSet FAST_STRING
preludeNames =
 mkUniqSet
   [ gHC__
   , pRELUDE   , pREL_BASE
   , pREL_READ , pREL_NUM
   , pREL_LIST , pREL_TUP
   , pACKED_STRING  , cONC_BASE
   , iO_BASE   , mONAD
   , rATIO     , iX
   , sT_BASE   , aRR_BASE
   , fOREIGN   , mAIN
   , gHC_MAIN  , gHC_ERR
   ]
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
