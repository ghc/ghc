%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SMRep]{Storage manager representations of closure}

This is here, rather than in ClosureInfo, just to keep nhc happy.
Other modules should access this info through ClosureInfo.

\begin{code}
module SMRep (
	SMRep(..), ClosureType(..),
	isStaticRep,
	fixedHdrSize, arrWordsHdrSize, arrPtrsHdrSize,
        stdItblSize, retItblSize,
	getSMRepClosureTypeInt,

	rET_SMALL, rET_VEC_SMALL, rET_BIG, rET_VEC_BIG,

	StgWord, StgHalfWord, hALF_WORD,
    ) where

#include "HsVersions.h"
#include "../includes/MachDeps.h"

import CmdLineOpts
import Constants
import Outputable

import DATA_WORD
\end{code}

%************************************************************************
%*									*
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
%*									*
%************************************************************************

\begin{code}
data SMRep
     -- static closure have an extra static link field at the end.
  = GenericRep		-- GC routines consult sizes in info tbl
	Bool		-- True <=> This is a static closure.  Affects how 
			-- 	    we garbage-collect it
	!Int		-- # ptr words
	!Int		-- # non-ptr words
	ClosureType	-- closure type

  | BlackHoleRep

data ClosureType	-- Corresponds 1-1 with the varieties of closures
			-- implemented by the RTS.  Compare with ghc/includes/ClosureTypes.h
    = Constr
    | ConstrNoCaf
    | Fun
    | Thunk
    | ThunkSelector
\end{code}

Size of a closure header.

\begin{code}
fixedHdrSize :: Int{-words-}
fixedHdrSize = sTD_HDR_SIZE + profHdrSize + granHdrSize

profHdrSize  :: Int{-words-}
profHdrSize  | opt_SccProfilingOn   = pROF_HDR_SIZE
	     | otherwise	    = 0

granHdrSize  :: Int{-words-}
granHdrSize  | opt_GranMacros	    = gRAN_HDR_SIZE
	     | otherwise	    = 0

arrWordsHdrSize   :: Int{-words-}
arrWordsHdrSize   = fixedHdrSize + aRR_WORDS_HDR_SIZE

arrPtrsHdrSize   :: Int{-words-}
arrPtrsHdrSize   = fixedHdrSize + aRR_PTRS_HDR_SIZE
\end{code}

Size of an info table.

\begin{code}
stdItblSize :: Int{-words-}
stdItblSize = sTD_ITBL_SIZE + profItblSize + granItblSize + tickyItblSize

retItblSize :: Int{-words-}
retItblSize = stdItblSize + rET_ITBL_SIZE

profItblSize  :: Int{-words-}
profItblSize  | opt_SccProfilingOn  = pROF_ITBL_SIZE
	      | otherwise	    = 0

granItblSize  :: Int{-words-}
granItblSize  | opt_GranMacros	    = gRAN_ITBL_SIZE
	      | otherwise	    = 0

tickyItblSize :: Int{-words-}
tickyItblSize | opt_DoTickyProfiling = tICKY_ITBL_SIZE
	      | otherwise	     = 0
\end{code}

\begin{code}
isStaticRep :: SMRep -> Bool
isStaticRep (GenericRep is_static _ _ _) = is_static
isStaticRep BlackHoleRep	         = False
\end{code}

\begin{code}
#include "../includes/ClosureTypes.h"
-- Defines CONSTR, CONSTR_1_0 etc

getSMRepClosureTypeInt :: SMRep -> Int
getSMRepClosureTypeInt (GenericRep False 1 0 Constr) = CONSTR_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 Constr) = CONSTR_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 Constr) = CONSTR_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 Constr) = CONSTR_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 Constr) = CONSTR_0_2
getSMRepClosureTypeInt (GenericRep False _ _ Constr) = CONSTR

getSMRepClosureTypeInt (GenericRep False 1 0 Fun) = FUN_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 Fun) = FUN_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 Fun) = FUN_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 Fun) = FUN_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 Fun) = FUN_0_2
getSMRepClosureTypeInt (GenericRep False _ _ Fun) = FUN

getSMRepClosureTypeInt (GenericRep False 1 0 Thunk) = THUNK_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 Thunk) = THUNK_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 Thunk) = THUNK_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 Thunk) = THUNK_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 Thunk) = THUNK_0_2
getSMRepClosureTypeInt (GenericRep False _ _ Thunk) = THUNK

getSMRepClosureTypeInt (GenericRep False _ _ ThunkSelector) =  THUNK_SELECTOR

getSMRepClosureTypeInt (GenericRep True _ _ Constr)      = CONSTR_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ ConstrNoCaf) = CONSTR_NOCAF_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ Fun)         = FUN_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ Thunk)       = THUNK_STATIC

getSMRepClosureTypeInt BlackHoleRep = BLACKHOLE

getSMRepClosureTypeInt rep = panic "getSMRepClosureTypeInt"


-- We export these ones
rET_SMALL     = (RET_SMALL     :: Int)
rET_VEC_SMALL = (RET_VEC_SMALL :: Int)
rET_BIG       = (RET_BIG       :: Int)
rET_VEC_BIG   = (RET_VEC_BIG   :: Int)
\end{code}

A type representing an StgWord on the target platform.

\begin{code}
#if SIZEOF_HSWORD == 4
type StgWord     = Word32
type StgHalfWord = Word16
hALF_WORD = 16 :: Int
#elif SIZEOF_HSWORD == 8
type StgWord     = Word64
type StgHalfWord = Word32
hALF_WORD = 32 :: Int
#else
#error unknown SIZEOF_HSWORD
#endif
\end{code}
