%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[Constants]{Info about this compilation}

!!!!! THIS CODE MUST AGREE WITH SMinterface.h !!!!!!

*** This SHOULD BE the only module that is CPP'd with "stgdefs.h" stuff.

\begin{code}
#include "HsVersions.h"

module Constants (
	uNFOLDING_USE_THRESHOLD,
	uNFOLDING_CREATION_THRESHOLD,
--	uNFOLDING_OVERRIDE_THRESHOLD,
	iNTERFACE_UNFOLD_THRESHOLD,
	lIBERATE_CASE_THRESHOLD,
	uNFOLDING_CHEAP_OP_COST,
	uNFOLDING_DEAR_OP_COST,
	uNFOLDING_NOREP_LIT_COST,
	uNFOLDING_CON_DISCOUNT_WEIGHT,

	mAX_SPEC_ALL_PTRS,
	mAX_SPEC_ALL_NONPTRS,
	mAX_SPEC_MIXED_FIELDS,
	mAX_SPEC_SELECTEE_SIZE,

	tARGET_MIN_INT, tARGET_MAX_INT,

	mIN_UPD_SIZE,
	mIN_SIZE_NonUpdHeapObject,
	mIN_SIZE_NonUpdStaticHeapObject,

	mAX_FAMILY_SIZE_FOR_VEC_RETURNS,

	sTD_UF_SIZE,	 cON_UF_SIZE,
	sCC_STD_UF_SIZE, sCC_CON_UF_SIZE,
	uF_RET,
	uF_SUB,
	uF_SUA,
	uF_UPDATEE,
	uF_COST_CENTRE,

	mAX_Vanilla_REG,
	mAX_Float_REG,
	mAX_Double_REG,

	mIN_BIG_TUPLE_SIZE,

    	mIN_MP_INT_SIZE,
	mP_STRUCT_SIZE,

	oTHER_TAG, iND_TAG,	-- semi-tagging stuff

	lIVENESS_R1,
	lIVENESS_R2,
	lIVENESS_R3,
	lIVENESS_R4,
	lIVENESS_R5,
	lIVENESS_R6,
	lIVENESS_R7,
	lIVENESS_R8,

	mAX_INTLIKE, mIN_INTLIKE,


	spARelToInt,
	spBRelToInt
    ) where

-- This magical #include brings in all the everybody-knows-these magic
-- constants unfortunately, we need to be *explicit* about which one
-- we want; if we just hope a -I... will get the right one, we could
-- be in trouble.

#include "../../includes/GhcConstants.h"

CHK_Ubiq() -- debugging consistency check

import Util
\end{code}

All pretty arbitrary:
\begin{code}
uNFOLDING_USE_THRESHOLD	      = ( 8 :: Int)
uNFOLDING_CREATION_THRESHOLD  = (30 :: Int)
iNTERFACE_UNFOLD_THRESHOLD    = (30 :: Int)
lIBERATE_CASE_THRESHOLD	      = (10 :: Int)
-- uNFOLDING_OVERRIDE_THRESHOLD  = ( 8 :: Int)

uNFOLDING_CHEAP_OP_COST       = ( 1 :: Int)
uNFOLDING_DEAR_OP_COST        = ( 4 :: Int)
uNFOLDING_NOREP_LIT_COST      = ( 20 :: Int)	-- Strings can be pretty big
uNFOLDING_CON_DISCOUNT_WEIGHT = ( 1 :: Int)
\end{code}

\begin{code}
mAX_SPEC_ALL_PTRS	= (MAX_SPEC_ALL_PTRS :: Int)
mAX_SPEC_ALL_NONPTRS	= (MAX_SPEC_ALL_NONPTRS :: Int)
mAX_SPEC_MIXED_FIELDS	= (MAX_SPEC_OTHER_SIZE :: Int)
mAX_SPEC_SELECTEE_SIZE	= (MAX_SPEC_SELECTEE_SIZE :: Int)

-- closure sizes: these do NOT include the header
mIN_UPD_SIZE			= (MIN_UPD_SIZE::Int)
mIN_SIZE_NonUpdHeapObject	= (MIN_NONUPD_SIZE::Int)
mIN_SIZE_NonUpdStaticHeapObject	= (0::Int)
\end{code}

A completely random number:
\begin{code}
mIN_BIG_TUPLE_SIZE = (16::Int)
\end{code}

Sizes of gmp objects:
\begin{code}
mIN_MP_INT_SIZE = (MIN_MP_INT_SIZE :: Int)
mP_STRUCT_SIZE = (MP_STRUCT_SIZE :: Int)
\end{code}

\begin{code}
tARGET_MIN_INT, tARGET_MAX_INT :: Integer
tARGET_MIN_INT = -536870912
tARGET_MAX_INT =  536870912
\end{code}
 
Constants for semi-tagging; the tags associated with the data
constructors will start at 0 and go up.
\begin{code}
oTHER_TAG = (INFO_OTHER_TAG :: Integer)	-- (-1) unevaluated, probably
iND_TAG	  = (INFO_IND_TAG   :: Integer) -- (-2) NOT USED, REALLY
\end{code}

Stuff for liveness masks:
\begin{code}
lIVENESS_R1	= (LIVENESS_R1 :: Int)
lIVENESS_R2	= (LIVENESS_R2 :: Int)
lIVENESS_R3	= (LIVENESS_R3 :: Int)
lIVENESS_R4	= (LIVENESS_R4 :: Int)
lIVENESS_R5	= (LIVENESS_R5 :: Int)
lIVENESS_R6	= (LIVENESS_R6 :: Int)
lIVENESS_R7	= (LIVENESS_R7 :: Int)
lIVENESS_R8	= (LIVENESS_R8 :: Int)
\end{code}

\begin{code}
mIN_INTLIKE, mAX_INTLIKE :: Integer	-- Only used to compare with (MachInt Integer)
mIN_INTLIKE = MIN_INTLIKE
mAX_INTLIKE = MAX_INTLIKE
\end{code}

\begin{code}
-- THESE ARE DIRECTION SENSITIVE!
spARelToInt :: Int{-VirtualSpAOffset-} -> Int{-VirtualSpAOffset-} -> Int
spBRelToInt :: Int{-VirtualSpBOffset-} -> Int{-VirtualSpBOffset-} -> Int

spARelToInt spA off = spA - off -- equiv to: AREL(spA - off)
spBRelToInt spB off = off - spB -- equiv to: BREL(spB - off)
\end{code}

A section of code-generator-related MAGIC CONSTANTS.
\begin{code}
mAX_FAMILY_SIZE_FOR_VEC_RETURNS = (MAX_VECTORED_RTN::Int)  -- pretty arbitrary
-- If you change this, you may need to change runtimes/standard/Update.lhc

-- The update frame sizes
sTD_UF_SIZE	= (NOSCC_STD_UF_SIZE::Int)
cON_UF_SIZE	= (NOSCC_CON_UF_SIZE::Int)

-- Same again, with profiling
sCC_STD_UF_SIZE	= (SCC_STD_UF_SIZE::Int)
sCC_CON_UF_SIZE	= (SCC_CON_UF_SIZE::Int)

-- Offsets in an update frame.  They don't change with profiling!
uF_RET = (UF_RET::Int)
uF_SUB = (UF_SUB::Int)
uF_SUA = (UF_SUA::Int)
uF_UPDATEE = (UF_UPDATEE::Int)
uF_COST_CENTRE = (UF_COST_CENTRE::Int)
\end{code}

\begin{code}
mAX_Vanilla_REG	= (MAX_VANILLA_REG :: Int)
mAX_Float_REG	= (MAX_FLOAT_REG :: Int)
mAX_Double_REG	= (MAX_DOUBLE_REG :: Int)
\end{code}
