%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SMRep]{Storage manager representations of closure}

This is here, rather than in ClosureInfo, just to keep nhc happy.
Other modules should access this info through ClosureInfo.

\begin{code}
module SMRep (
	SMRep(..), ClosureType(..),
	isConstantRep, isStaticRep,
	fixedHdrSize, arrHdrSize, fixedItblSize, pprSMRep

#ifndef OMIT_NATIVE_CODEGEN
	, getSMRepClosureTypeInt
	, cONSTR                  
	, cONSTR_STATIC           
	, cONSTR_NOCAF_STATIC     
	, fUN                     
	, fUN_STATIC              
	, tHUNK                   
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
			  gRAN_HDR_SIZE, tICKY_HDR_SIZE, aRR_HDR_SIZE,
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
  = StaticRep
	Int		-- # ptr words (useful for interpreter, debugger, etc)
	Int		-- # non-ptr words
	ClosureType     -- closure type

  | GenericRep		-- GC routines consult sizes in info tbl
	Int		-- # ptr words
	Int		-- # non-ptr words
	ClosureType	-- closure type

  | ConstantRep		-- CONSTR with zero-arity

  | BlackHoleRep

data ClosureType
    = CONSTR
    | CONSTR_p_n Int Int
    | CONSTR_NOCAF
    | FUN
    | FUN_p_n Int Int
    | THUNK
    | THUNK_p_n Int Int
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

arrHdrSize   :: Int{-words-}
arrHdrSize   = fixedHdrSize + aRR_HDR_SIZE
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
isConstantRep, isStaticRep :: SMRep -> Bool
isConstantRep ConstantRep     = True
isConstantRep other 	      = False

isStaticRep (StaticRep _ _ _) = True
isStaticRep _		      = False
\end{code}

\begin{code}
{- ToDo: needed? -}
instance Text SMRep where
    showsPrec d rep
      = showString (case rep of
	   StaticRep _ _ _   	                 -> "STATIC"
	   GenericRep _ _ _   	                 -> ""
	   ConstantRep				 -> "")

instance Outputable SMRep where
    ppr rep = pprSMRep rep

pprSMRep :: SMRep -> SDoc
pprSMRep (GenericRep _ _ t) 	= pprClosureType t
pprSMRep (StaticRep _ _ t)  	= pprClosureType t <> ptext SLIT("_STATIC")
pprSMRep ConstantRep        	= ptext SLIT("CONSTR_NOCAF_STATIC")
pprSMRep BlackHoleRep       	= ptext SLIT("BLACKHOLE")

pprClosureType CONSTR	   	= ptext SLIT("CONSTR")
pprClosureType (CONSTR_p_n p n) = ptext SLIT("CONSTR_") <> int p <> char '_' <> int n
pprClosureType CONSTR_NOCAF	= ptext SLIT("CONSTR_NOCAF")
pprClosureType FUN		= ptext SLIT("FUN")
pprClosureType (FUN_p_n p n)	= ptext SLIT("FUN_") <> int p <> char '_' <> int n
pprClosureType THUNK		= ptext SLIT("THUNK")
pprClosureType (THUNK_p_n p n)  = ptext SLIT("THUNK_") <> int p <> char '_' <> int n
pprClosureType THUNK_SELECTOR   = ptext SLIT("THUNK_SELECTOR")

#ifndef OMIT_NATIVE_CODEGEN
getSMRepClosureTypeInt :: SMRep -> Int
getSMRepClosureTypeInt (GenericRep _ _ t) =
  case t of 
    CONSTR 	   -> cONSTR
    CONSTR_NOCAF   -> panic "getClosureTypeInt: CONSTR_NOCAF"
    FUN    	   -> fUN
    THUNK  	   -> tHUNK
    THUNK_SELECTOR -> tHUNK_SELECTOR
getSMRepClosureTypeInt (StaticRep _ _ t) =
  case t of 
    CONSTR 	   -> cONSTR_STATIC
    CONSTR_NOCAF   -> cONSTR_NOCAF_STATIC
    FUN    	   -> fUN_STATIC
    THUNK  	   -> tHUNK_STATIC
    THUNK_SELECTOR -> panic "getClosureTypeInt: THUNK_SELECTOR_STATIC"

getSMRepClosureTypeInt ConstantRep = cONSTR_NOCAF_STATIC

getSMRepClosureTypeInt BlackHoleRep = bLACKHOLE

-- Just the ones we need:

#include "../includes/ClosureTypes.h"

cONSTR                  = (CONSTR               :: Int)
cONSTR_STATIC           = (CONSTR_STATIC        :: Int)
cONSTR_NOCAF_STATIC     = (CONSTR_NOCAF_STATIC  :: Int)
fUN                     = (FUN                  :: Int)
fUN_STATIC              = (FUN_STATIC           :: Int)
tHUNK                   = (THUNK                :: Int)
tHUNK_STATIC            = (THUNK_STATIC         :: Int)
tHUNK_SELECTOR          = (THUNK_SELECTOR       :: Int)
rET_SMALL               = (RET_SMALL            :: Int)
rET_VEC_SMALL           = (RET_VEC_SMALL        :: Int)
rET_BIG                 = (RET_BIG              :: Int)
rET_VEC_BIG             = (RET_VEC_BIG          :: Int)
bLACKHOLE               = (BLACKHOLE            :: Int)

#endif OMIT_NATIVE_CODEGEN
\end{code}
