%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SMRep]{Storage manager representations of closure}

This is here, rather than in ClosureInfo, just to keep nhc happy.
Other modules should access this info through ClosureInfo.

\begin{code}
module SMRep (
	-- Words and bytes
	StgWord, StgHalfWord, 
	hALF_WORD_SIZE, hALF_WORD_SIZE_IN_BITS,
	WordOff, ByteOff,

	-- Argument/return representations
	CgRep(..), nonVoidArg,
	argMachRep, primRepToCgRep, primRepHint,
	isFollowableArg, isVoidArg, 
	isFloatingArg, isNonPtrArg, is64BitArg,
	separateByPtrFollowness,
	cgRepSizeW, cgRepSizeB,
	retAddrSizeW,

	typeCgRep, idCgRep, tyConCgRep, typeHint,

	-- Closure repesentation
	SMRep(..), ClosureType(..),
	isStaticRep,
	fixedHdrSize, arrWordsHdrSize, arrPtrsHdrSize,
	profHdrSize,
	tablesNextToCode,
	smRepClosureType, smRepClosureTypeInt,

	rET_SMALL, rET_VEC_SMALL, rET_BIG, rET_VEC_BIG
    ) where

#include "HsVersions.h"
#include "../includes/MachDeps.h"

import Id		( Id, idType )
import Type		( Type, typePrimRep, PrimRep(..) )
import TyCon		( TyCon, tyConPrimRep )
import MachOp--		( MachRep(..), MachHint(..), wordRep )
import StaticFlags	( opt_SccProfilingOn, opt_GranMacros,
			  opt_Unregisterised )
import Constants
import Outputable

import DATA_WORD
\end{code}


%************************************************************************
%*									*
		Words and bytes
%*									*
%************************************************************************

\begin{code}
type WordOff = Int	-- Word offset, or word count
type ByteOff = Int	-- Byte offset, or byte count
\end{code}

StgWord is a type representing an StgWord on the target platform.

\begin{code}
#if SIZEOF_HSWORD == 4
type StgWord     = Word32
type StgHalfWord = Word16
hALF_WORD_SIZE = 2 :: ByteOff
hALF_WORD_SIZE_IN_BITS = 16 :: Int
#elif SIZEOF_HSWORD == 8
type StgWord     = Word64
type StgHalfWord = Word32
hALF_WORD_SIZE = 4 :: ByteOff
hALF_WORD_SIZE_IN_BITS = 32 :: Int
#else
#error unknown SIZEOF_HSWORD
#endif
\end{code}


%************************************************************************
%*									*
			CgRep
%*									*
%************************************************************************

An CgRep is an abstraction of a Type which tells the code generator
all it needs to know about the calling convention for arguments (and
results) of that type.  In particular, the ArgReps of a function's
arguments are used to decide which of the RTS's generic apply
functions to call when applying an unknown function.

It contains more information than the back-end data type MachRep,
so one can easily convert from CgRep -> MachRep.  (Except that
there's no MachRep for a VoidRep.)

It distinguishes 
	pointers from non-pointers (we sort the pointers together
	when building closures)

	void from other types: a void argument is different from no argument

All 64-bit types map to the same CgRep, because they're passed in the
same register, but a PtrArg is still different from an NonPtrArg
because the function's entry convention has to take into account the
pointer-hood of arguments for the purposes of describing the stack on
entry to the garbage collector.

\begin{code}
data CgRep 
  = VoidArg 	-- Void
  | PtrArg 	-- Word-sized Ptr
  | NonPtrArg 	-- Word-sized non-pointer
  | LongArg	-- 64-bit non-pointer
  | FloatArg 	-- 32-bit float
  | DoubleArg 	-- 64-bit float
  deriving Eq

instance Outputable CgRep where
    ppr VoidArg   = ptext SLIT("V_")
    ppr PtrArg    = ptext SLIT("P_")
    ppr NonPtrArg = ptext SLIT("I_")
    ppr LongArg   = ptext SLIT("L_")
    ppr FloatArg  = ptext SLIT("F_")
    ppr DoubleArg = ptext SLIT("D_")

argMachRep :: CgRep -> MachRep
argMachRep PtrArg    = wordRep
argMachRep NonPtrArg = wordRep
argMachRep LongArg   = I64
argMachRep FloatArg  = F32
argMachRep DoubleArg = F64
argMachRep VoidArg   = panic "argMachRep:VoidRep"

primRepToCgRep :: PrimRep -> CgRep
primRepToCgRep VoidRep    = VoidArg
primRepToCgRep PtrRep     = PtrArg
primRepToCgRep IntRep	  = NonPtrArg
primRepToCgRep WordRep	  = NonPtrArg
primRepToCgRep Int64Rep   = LongArg
primRepToCgRep Word64Rep  = LongArg
primRepToCgRep AddrRep    = NonPtrArg
primRepToCgRep FloatRep   = FloatArg
primRepToCgRep DoubleRep  = DoubleArg

primRepHint :: PrimRep -> MachHint
primRepHint VoidRep	= panic "primRepHint:VoidRep"
primRepHint PtrRep	= PtrHint
primRepHint IntRep	= SignedHint
primRepHint WordRep	= NoHint
primRepHint Int64Rep	= SignedHint
primRepHint Word64Rep	= NoHint
primRepHint AddrRep     = PtrHint -- NB! PtrHint, but NonPtrArg
primRepHint FloatRep	= FloatHint
primRepHint DoubleRep	= FloatHint

idCgRep :: Id -> CgRep
idCgRep = typeCgRep . idType

tyConCgRep :: TyCon -> CgRep
tyConCgRep = primRepToCgRep . tyConPrimRep

typeCgRep :: Type -> CgRep
typeCgRep = primRepToCgRep . typePrimRep

typeHint :: Type -> MachHint
typeHint = primRepHint . typePrimRep
\end{code}

Whether or not the thing is a pointer that the garbage-collector
should follow. Or, to put it another (less confusing) way, whether
the object in question is a heap object. 

Depending on the outcome, this predicate determines what stack
the pointer/object possibly will have to be saved onto, and the
computation of GC liveness info.

\begin{code}
isFollowableArg :: CgRep -> Bool  -- True <=> points to a heap object
isFollowableArg PtrArg  = True
isFollowableArg other = False

isVoidArg :: CgRep -> Bool
isVoidArg VoidArg = True
isVoidArg other   = False

nonVoidArg :: CgRep -> Bool
nonVoidArg VoidArg = False
nonVoidArg other   = True

-- isFloatingArg is used to distinguish @Double@ and @Float@ which
-- cause inadvertent numeric conversions if you aren't jolly careful.
-- See codeGen/CgCon:cgTopRhsCon.

isFloatingArg :: CgRep -> Bool
isFloatingArg DoubleArg = True
isFloatingArg FloatArg  = True
isFloatingArg _         = False

isNonPtrArg :: CgRep -> Bool
-- Identify anything which is one word large and not a pointer.
isNonPtrArg NonPtrArg = True
isNonPtrArg other     = False

is64BitArg :: CgRep -> Bool
is64BitArg LongArg = True
is64BitArg _       = False
\end{code}

\begin{code}
separateByPtrFollowness :: [(CgRep,a)] -> ([(CgRep,a)], [(CgRep,a)])
-- Returns (ptrs, non-ptrs)
separateByPtrFollowness things
  = sep_things things [] []
    -- accumulating params for follow-able and don't-follow things...
  where
    sep_things []    	       bs us = (reverse bs, reverse us)
    sep_things ((PtrArg,a):ts) bs us = sep_things ts ((PtrArg,a):bs) us
    sep_things (t         :ts) bs us = sep_things ts bs		     (t:us)
\end{code}

\begin{code}
cgRepSizeB :: CgRep -> ByteOff
cgRepSizeB DoubleArg = dOUBLE_SIZE
cgRepSizeB LongArg   = wORD64_SIZE
cgRepSizeB VoidArg   = 0
cgRepSizeB _         = wORD_SIZE

cgRepSizeW :: CgRep -> ByteOff
cgRepSizeW DoubleArg = dOUBLE_SIZE `quot` wORD_SIZE
cgRepSizeW LongArg   = wORD64_SIZE `quot` wORD_SIZE
cgRepSizeW VoidArg   = 0
cgRepSizeW _         = 1

retAddrSizeW :: WordOff
retAddrSizeW = 1	-- One word
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
	!Int		--  # ptr words
	!Int		--  # non-ptr words
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
fixedHdrSize :: WordOff
fixedHdrSize = sTD_HDR_SIZE + profHdrSize + granHdrSize

profHdrSize  :: WordOff
profHdrSize  | opt_SccProfilingOn   = pROF_HDR_SIZE
	     | otherwise	    = 0

granHdrSize  :: WordOff
granHdrSize  | opt_GranMacros	    = gRAN_HDR_SIZE
	     | otherwise	    = 0

arrWordsHdrSize   :: ByteOff
arrWordsHdrSize   = fixedHdrSize*wORD_SIZE + sIZEOF_StgArrWords_NoHdr

arrPtrsHdrSize    :: ByteOff
arrPtrsHdrSize    = fixedHdrSize*wORD_SIZE + sIZEOF_StgMutArrPtrs_NoHdr
\end{code}

\begin{code}
-- IA64 mangler doesn't place tables next to code
tablesNextToCode :: Bool
#if defined(ia64_TARGET_ARCH) || defined(powerpc64_TARGET_ARCH)
tablesNextToCode = False
#else
tablesNextToCode = not opt_Unregisterised
#endif
\end{code}

\begin{code}
isStaticRep :: SMRep -> Bool
isStaticRep (GenericRep is_static _ _ _) = is_static
isStaticRep BlackHoleRep	         = False
\end{code}

\begin{code}
#include "../includes/ClosureTypes.h"
-- Defines CONSTR, CONSTR_1_0 etc


smRepClosureType :: SMRep -> ClosureType
smRepClosureType (GenericRep _ _ _ ty) = ty
smRepClosureType BlackHoleRep	       = panic "smRepClosureType: black hole"

smRepClosureTypeInt :: SMRep -> Int
smRepClosureTypeInt (GenericRep False 1 0 Constr) = CONSTR_1_0
smRepClosureTypeInt (GenericRep False 0 1 Constr) = CONSTR_0_1
smRepClosureTypeInt (GenericRep False 2 0 Constr) = CONSTR_2_0
smRepClosureTypeInt (GenericRep False 1 1 Constr) = CONSTR_1_1
smRepClosureTypeInt (GenericRep False 0 2 Constr) = CONSTR_0_2
smRepClosureTypeInt (GenericRep False _ _ Constr) = CONSTR

smRepClosureTypeInt (GenericRep False 1 0 Fun) = FUN_1_0
smRepClosureTypeInt (GenericRep False 0 1 Fun) = FUN_0_1
smRepClosureTypeInt (GenericRep False 2 0 Fun) = FUN_2_0
smRepClosureTypeInt (GenericRep False 1 1 Fun) = FUN_1_1
smRepClosureTypeInt (GenericRep False 0 2 Fun) = FUN_0_2
smRepClosureTypeInt (GenericRep False _ _ Fun) = FUN

smRepClosureTypeInt (GenericRep False 1 0 Thunk) = THUNK_1_0
smRepClosureTypeInt (GenericRep False 0 1 Thunk) = THUNK_0_1
smRepClosureTypeInt (GenericRep False 2 0 Thunk) = THUNK_2_0
smRepClosureTypeInt (GenericRep False 1 1 Thunk) = THUNK_1_1
smRepClosureTypeInt (GenericRep False 0 2 Thunk) = THUNK_0_2
smRepClosureTypeInt (GenericRep False _ _ Thunk) = THUNK

smRepClosureTypeInt (GenericRep False _ _ ThunkSelector) =  THUNK_SELECTOR

smRepClosureTypeInt (GenericRep True _ _ Constr)      = CONSTR_STATIC
smRepClosureTypeInt (GenericRep True _ _ ConstrNoCaf) = CONSTR_NOCAF_STATIC
smRepClosureTypeInt (GenericRep True _ _ Fun)         = FUN_STATIC
smRepClosureTypeInt (GenericRep True _ _ Thunk)       = THUNK_STATIC

smRepClosureTypeInt BlackHoleRep = BLACKHOLE

smRepClosureTypeInt rep = panic "smRepClosuretypeint"


-- We export these ones
rET_SMALL     = (RET_SMALL     :: Int)
rET_VEC_SMALL = (RET_VEC_SMALL :: Int)
rET_BIG       = (RET_BIG       :: Int)
rET_VEC_BIG   = (RET_VEC_BIG   :: Int)
\end{code}

