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
        fixedItblSize, pprSMRep

#ifndef OMIT_NATIVE_CODEGEN
	, getSMRepClosureTypeInt
	, cONSTR
	, cONSTR_1_0
	, cONSTR_0_1
	, cONSTR_2_0
	, cONSTR_1_1
	, cONSTR_0_2
	, cONSTR_STATIC
	, cONSTR_NOCAF_STATIC
	, fUN
	, fUN_1_0
	, fUN_0_1
	, fUN_2_0
	, fUN_1_1
	, fUN_0_2
	, fUN_STATIC
	, tHUNK
	, tHUNK_1_0
	, tHUNK_0_1
	, tHUNK_2_0
	, tHUNK_1_1
	, tHUNK_0_2
	, tHUNK_STATIC
	, tHUNK_SELECTOR
	, rET_SMALL
	, rET_VEC_SMALL
	, rET_BIG
	, rET_VEC_BIG
	, bLACKHOLE
#endif
    ) where

#include "HsVersions.h"

import CmdLineOpts
import AbsCSyn		( Liveness(..) )
import Constants	( sTD_HDR_SIZE, pROF_HDR_SIZE,
			  gRAN_HDR_SIZE, tICKY_HDR_SIZE, 
                          aRR_WORDS_HDR_SIZE, aRR_PTRS_HDR_SIZE,
			  sTD_ITBL_SIZE, pROF_ITBL_SIZE,
			  gRAN_ITBL_SIZE, tICKY_ITBL_SIZE )
import Outputable
import GlaExts		( Int(..), Int#, (<#), (==#), (<#), (>#) )
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
	Int		-- # ptr words
	Int		-- # non-ptr words
	ClosureType	-- closure type

  | BlackHoleRep

data ClosureType	-- Corresponds 1-1 with the varieties of closures
			-- implemented by the RTS.  Compare with ghc/includes/ClosureTypes.h
    = CONSTR
    | CONSTR_p_n	-- The p_n variants have more efficient GC, but we
			-- only provide them for dynamically-allocated closures
			-- (We could do them for static ones, but we don't)
    | CONSTR_NOCAF
    | FUN
    | FUN_p_n
    | THUNK
    | THUNK_p_n
    | THUNK_SELECTOR
  deriving (Eq,Ord)
\end{code}

Size of a closure header.

\begin{code}
fixedHdrSize :: Int{-words-}
fixedHdrSize = sTD_HDR_SIZE + profHdrSize + granHdrSize + tickyHdrSize

profHdrSize  :: Int{-words-}
profHdrSize  | opt_SccProfilingOn   = pROF_HDR_SIZE
	     | otherwise	    = 0

granHdrSize  :: Int{-words-}
granHdrSize  | opt_GranMacros	    = gRAN_HDR_SIZE
	     | otherwise	    = 0

tickyHdrSize :: Int{-words-}
tickyHdrSize | opt_DoTickyProfiling = tICKY_HDR_SIZE
	     | otherwise	    = 0

arrWordsHdrSize   :: Int{-words-}
arrWordsHdrSize   = fixedHdrSize + aRR_WORDS_HDR_SIZE

arrPtrsHdrSize   :: Int{-words-}
arrPtrsHdrSize   = fixedHdrSize + aRR_PTRS_HDR_SIZE
\end{code}

Size of an info table.

\begin{code}
fixedItblSize :: Int{-words-}
fixedItblSize = sTD_ITBL_SIZE + profItblSize + granItblSize + tickyItblSize

profItblSize  :: Int{-words-}
profItblSize  | opt_SccProfilingOn   = pROF_ITBL_SIZE
	      | otherwise	    = 0

granItblSize  :: Int{-words-}
granItblSize  | opt_GranMacros	    = gRAN_ITBL_SIZE
	      | otherwise	    = 0

tickyItblSize :: Int{-words-}
tickyItblSize | opt_DoTickyProfiling = tICKY_ITBL_SIZE
	      | otherwise	    = 0
\end{code}

\begin{code}
isStaticRep :: SMRep -> Bool
isStaticRep (GenericRep is_static _ _ _) = is_static
isStaticRep BlackHoleRep	         = False
\end{code}

\begin{code}
instance Outputable SMRep where
    ppr rep = pprSMRep rep

pprSMRep :: SMRep -> SDoc
pprSMRep (GenericRep True  ptrs nptrs clo_ty) = pprClosureType clo_ty ptrs nptrs <> ptext SLIT("_STATIC")
pprSMRep (GenericRep False ptrs nptrs clo_ty) = pprClosureType clo_ty ptrs nptrs

pprClosureType CONSTR	      p n = ptext SLIT("CONSTR")
pprClosureType CONSTR_p_n     p n = ptext SLIT("CONSTR_") <> int p <> char '_' <> int n
pprClosureType CONSTR_NOCAF   p n = ptext SLIT("CONSTR_NOCAF")
pprClosureType FUN	      p n = ptext SLIT("FUN")
pprClosureType FUN_p_n        p n = ptext SLIT("FUN_") <> int p <> char '_' <> int n
pprClosureType THUNK	      p n = ptext SLIT("THUNK")
pprClosureType THUNK_p_n      p n = ptext SLIT("THUNK_") <> int p <> char '_' <> int n
pprClosureType THUNK_SELECTOR p n = ptext SLIT("THUNK_SELECTOR")

#ifndef OMIT_NATIVE_CODEGEN
getSMRepClosureTypeInt :: SMRep -> Int
getSMRepClosureTypeInt (GenericRep False _ _ CONSTR)     = cONSTR
getSMRepClosureTypeInt (GenericRep False 1 0 CONSTR_p_n) = cONSTR_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 CONSTR_p_n) = cONSTR_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 CONSTR_p_n) = cONSTR_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 CONSTR_p_n) = cONSTR_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 CONSTR_p_n) = cONSTR_0_2

getSMRepClosureTypeInt (GenericRep False _ _ FUN)     = fUN
getSMRepClosureTypeInt (GenericRep False 1 0 FUN_p_n) = fUN_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 FUN_p_n) = fUN_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 FUN_p_n) = fUN_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 FUN_p_n) = fUN_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 FUN_p_n) = fUN_0_2

getSMRepClosureTypeInt (GenericRep False _ _ THUNK)     = tHUNK
getSMRepClosureTypeInt (GenericRep False 1 0 THUNK_p_n) = tHUNK_1_0
getSMRepClosureTypeInt (GenericRep False 0 1 THUNK_p_n) = tHUNK_0_1
getSMRepClosureTypeInt (GenericRep False 2 0 THUNK_p_n) = tHUNK_2_0
getSMRepClosureTypeInt (GenericRep False 1 1 THUNK_p_n) = tHUNK_1_1
getSMRepClosureTypeInt (GenericRep False 0 2 THUNK_p_n) = tHUNK_0_2

getSMRepClosureTypeInt (GenericRep False _ _ THUNK_SELECTOR) =  tHUNK_SELECTOR

getSMRepClosureTypeInt (GenericRep True _ _ CONSTR)       = cONSTR_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ CONSTR_NOCAF) = cONSTR_NOCAF_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ FUN)          = fUN_STATIC
getSMRepClosureTypeInt (GenericRep True _ _ THUNK)        = tHUNK_STATIC

getSMRepClosureTypeInt BlackHoleRep = bLACKHOLE

getSMRepClosureTypeInt rep = pprPanic "getSMRepClosureTypeInt:" (pprSMRep rep)


-- Just the ones we need:

#include "../includes/ClosureTypes.h"

cONSTR                  = (CONSTR               :: Int)
cONSTR_1_0              = (CONSTR_1_0           :: Int)
cONSTR_0_1              = (CONSTR_0_1           :: Int)
cONSTR_2_0              = (CONSTR_2_0           :: Int)
cONSTR_1_1              = (CONSTR_1_1           :: Int)
cONSTR_0_2              = (CONSTR_0_2           :: Int)
cONSTR_STATIC           = (CONSTR_STATIC        :: Int)
cONSTR_NOCAF_STATIC     = (CONSTR_NOCAF_STATIC  :: Int)
fUN                     = (FUN                  :: Int)
fUN_1_0                 = (FUN_1_0              :: Int)
fUN_0_1                 = (FUN_0_1              :: Int)
fUN_2_0                 = (FUN_2_0              :: Int)
fUN_1_1                 = (FUN_1_1              :: Int)
fUN_0_2                 = (FUN_0_2              :: Int)
fUN_STATIC              = (FUN_STATIC           :: Int)
tHUNK                   = (THUNK                :: Int)
tHUNK_1_0               = (THUNK_1_0            :: Int)
tHUNK_0_1               = (THUNK_0_1            :: Int)
tHUNK_2_0               = (THUNK_2_0            :: Int)
tHUNK_1_1               = (THUNK_1_1            :: Int)
tHUNK_0_2               = (THUNK_0_2            :: Int)
tHUNK_STATIC            = (THUNK_STATIC         :: Int)
tHUNK_SELECTOR          = (THUNK_SELECTOR       :: Int)
rET_SMALL               = (RET_SMALL            :: Int)
rET_VEC_SMALL           = (RET_VEC_SMALL        :: Int)
rET_BIG                 = (RET_BIG              :: Int)
rET_VEC_BIG             = (RET_VEC_BIG          :: Int)
bLACKHOLE               = (BLACKHOLE            :: Int)

#endif OMIT_NATIVE_CODEGEN
\end{code}
