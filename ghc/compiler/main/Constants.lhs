%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Constants]{Info about this compilation}

\begin{code}
module Constants (
	mAX_CONTEXT_REDUCTION_DEPTH,
	mAX_TUPLE_SIZE,

	mAX_SPEC_THUNK_SIZE,
	mAX_SPEC_FUN_SIZE,
	mAX_SPEC_CONSTR_SIZE,
	mAX_SPEC_SELECTEE_SIZE,
	mAX_SPEC_AP_SIZE,

	mIN_UPD_SIZE,
	mIN_SIZE_NonUpdHeapObject,

	sTD_HDR_SIZE,
	pROF_HDR_SIZE,
	gRAN_HDR_SIZE,
	tICKY_HDR_SIZE,
	aRR_WORDS_HDR_SIZE,
	aRR_PTRS_HDR_SIZE,
        rESERVED_C_STACK_BYTES,

	sTD_ITBL_SIZE,
	pROF_ITBL_SIZE,
	gRAN_ITBL_SIZE,
	tICKY_ITBL_SIZE,

	mAX_FAMILY_SIZE_FOR_VEC_RETURNS,

	uF_SIZE,
	sCC_UF_SIZE,
	gRAN_UF_SIZE,  -- HWL
	uF_RET,
	uF_SU,
	uF_UPDATEE,
	uF_CCS,

	sEQ_FRAME_SIZE,
	sCC_SEQ_FRAME_SIZE,
	gRAN_SEQ_FRAME_SIZE, -- HWL

	mAX_Vanilla_REG,
	mAX_Float_REG,
	mAX_Double_REG,
	mAX_Long_REG,

	mAX_Real_Vanilla_REG,
	mAX_Real_Float_REG,
	mAX_Real_Double_REG,
	mAX_Real_Long_REG,

	oTHER_TAG,

	mAX_INTLIKE, mIN_INTLIKE,

	spRelToInt,

	dOUBLE_SIZE,
	iNT64_SIZE,
	wORD64_SIZE,
	
	bLOCK_SIZE,

	interfaceFileFormatVersion

    ) where

-- This magical #include brings in all the everybody-knows-these magic
-- constants unfortunately, we need to be *explicit* about which one
-- we want; if we just hope a -I... will get the right one, we could
-- be in trouble.

#include "HsVersions.h"
#include "../includes/config.h"
#include "../includes/MachRegs.h"
#include "../includes/Constants.h"

-- import Util
\end{code}

All pretty arbitrary:

\begin{code}
mAX_TUPLE_SIZE = (37 :: Int)
mAX_CONTEXT_REDUCTION_DEPTH = (20 :: Int)
\end{code}


\begin{code}
-- specialised fun/thunk/constr closure types
mAX_SPEC_THUNK_SIZE   = (MAX_SPEC_THUNK_SIZE :: Int)
mAX_SPEC_FUN_SIZE     = (MAX_SPEC_FUN_SIZE :: Int)
mAX_SPEC_CONSTR_SIZE  = (MAX_SPEC_CONSTR_SIZE :: Int)

-- pre-compiled thunk types
mAX_SPEC_SELECTEE_SIZE	= (MAX_SPEC_SELECTEE_SIZE :: Int)
mAX_SPEC_AP_SIZE        = (MAX_SPEC_AP_SIZE :: Int)

-- closure sizes: these do NOT include the header (see below for header sizes)
mIN_UPD_SIZE			= (MIN_UPD_SIZE::Int)
mIN_SIZE_NonUpdHeapObject	= (MIN_NONUPD_SIZE::Int)
\end{code}

Constants for semi-tagging; the tags associated with the data
constructors will start at 0 and go up.

\begin{code}
oTHER_TAG = (INFO_OTHER_TAG :: Integer)	-- (-1) unevaluated, probably
\end{code}

\begin{code}
mIN_INTLIKE, mAX_INTLIKE :: Integer	-- Only used to compare with (MachInt Integer)
mIN_INTLIKE = MIN_INTLIKE
mAX_INTLIKE = MAX_INTLIKE
\end{code}

A little function that abstracts the stack direction.  Note that most
of the code generator is dependent on the stack direction anyway, so
changing this on its own spells certain doom.  ToDo: remove?

\begin{code}
-- THIS IS DIRECTION SENSITIVE!

-- stack grows down, positive virtual offsets correspond to negative
-- additions to the stack pointer.

spRelToInt :: Int{-VirtualSpOffset-} -> Int{-VirtualSpOffset-} -> Int
spRelToInt sp off = sp - off
\end{code}

A section of code-generator-related MAGIC CONSTANTS.

\begin{code}
mAX_FAMILY_SIZE_FOR_VEC_RETURNS = (MAX_VECTORED_RTN::Int)  -- pretty arbitrary
-- If you change this, you may need to change runtimes/standard/Update.lhc

-- The update frame sizes
uF_SIZE	= (NOSCC_UF_SIZE::Int)

-- Same again, with profiling
sCC_UF_SIZE = (SCC_UF_SIZE::Int)

-- Same again, with gransim
gRAN_UF_SIZE = (GRAN_UF_SIZE::Int)

-- Offsets in an update frame.  They don't change with profiling!
uF_RET         = (UF_RET::Int)
uF_SU          = (UF_SU::Int)
uF_UPDATEE     = (UF_UPDATEE::Int)
uF_CCS         = (UF_CCS::Int)
\end{code}

Seq frame sizes.

\begin{code}
sEQ_FRAME_SIZE = (NOSCC_SEQ_FRAME_SIZE::Int)
sCC_SEQ_FRAME_SIZE = (SCC_SEQ_FRAME_SIZE::Int)
gRAN_SEQ_FRAME_SIZE = (GRAN_SEQ_FRAME_SIZE::Int)
\end{code}

\begin{code}
mAX_Vanilla_REG	= (MAX_VANILLA_REG :: Int)
mAX_Float_REG	= (MAX_FLOAT_REG :: Int)
mAX_Double_REG	= (MAX_DOUBLE_REG :: Int)
mAX_Long_REG    = (MAX_LONG_REG  :: Int)

mAX_Real_Vanilla_REG	= (MAX_REAL_VANILLA_REG :: Int)
mAX_Real_Float_REG	= (MAX_REAL_FLOAT_REG :: Int)
mAX_Real_Double_REG	= (MAX_REAL_DOUBLE_REG :: Int)
#ifdef MAX_REAL_LONG_REG
mAX_Real_Long_REG	= (MAX_REAL_LONG_REG :: Int)
#else
mAX_Real_Long_REG       = (0::Int)
#endif
\end{code}

Closure header sizes.

\begin{code}
sTD_HDR_SIZE       = (STD_HDR_SIZE       :: Int)
pROF_HDR_SIZE      = (PROF_HDR_SIZE      :: Int)
gRAN_HDR_SIZE      = (GRAN_HDR_SIZE      :: Int)
tICKY_HDR_SIZE     = (TICKY_HDR_SIZE     :: Int)
aRR_WORDS_HDR_SIZE = (ARR_WORDS_HDR_SIZE :: Int)
aRR_PTRS_HDR_SIZE  = (ARR_PTRS_HDR_SIZE  :: Int)
\end{code}

Info Table sizes.

\begin{code}
sTD_ITBL_SIZE   = (STD_ITBL_SIZE   :: Int)
pROF_ITBL_SIZE  = (PROF_ITBL_SIZE  :: Int)
gRAN_ITBL_SIZE  = (GRAN_ITBL_SIZE  :: Int)
tICKY_ITBL_SIZE = (TICKY_ITBL_SIZE :: Int)
\end{code}

Size of a double in StgWords.

\begin{code}
dOUBLE_SIZE    = (DOUBLE_SIZE   :: Int)
wORD64_SIZE    = (WORD64_SIZE   :: Int)
iNT64_SIZE     = (INT64_SIZE   :: Int)
\end{code}

The version of the interface file format we're
using:

\begin{code}
interfaceFileFormatVersion :: Int
interfaceFileFormatVersion = HscIfaceFileVersion
\end{code}

This tells the native code generator the size of the spill
area is has available.

\begin{code}
rESERVED_C_STACK_BYTES = (RESERVED_C_STACK_BYTES :: Int)
\end{code}

Size of a storage manager block (in bytes).

\begin{code}
bLOCK_SIZE = (BLOCK_SIZE :: Int)
\end{code}
