%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[PrimRep]{Primitive machine-level kinds of things.}

At various places in the back end, we want to be to tag things with a
``primitive kind''---i.e., the machine-manipulable implementation
types.

\begin{code}
module PrimRep 
      (
	PrimRep(..)
      , separateByPtrFollowness
      , isFollowableRep
      , isFloatingRep
      , is64BitRep
      , getPrimRepSize
      , getPrimRepSizeInBytes
      , getPrimRepArrayElemSize
      , retPrimRepSize
      ) where

#include "HsVersions.h"

import Constants ( dOUBLE_SIZE, iNT64_SIZE, wORD64_SIZE, wORD_SIZE )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[PrimRep-datatype]{The @PrimRep@ datatype}
%*									*
%************************************************************************

\begin{code}
data PrimRep
  = -- These pointer-kinds are all really the same, but we keep
    -- them separate for documentation purposes.
    PtrRep		-- Pointer to a closure; a ``word''.
  | CodePtrRep		-- Pointer to code
  | DataPtrRep		-- Pointer to data
  | RetRep 	    	-- Pointer to code or data (return vector or code pointer)
  | CostCentreRep	-- Pointer to a cost centre

  | CharRep		-- Machine characters
  | IntRep		--	   signed   integers (same size as ptr on this arch)
  | WordRep		--	   unsigned integers (same size as ptr on this arch)
  | AddrRep		--	   addresses (C pointers)
  | FloatRep		--	   floats
  | DoubleRep		--	   doubles

  | Int8Rep             --          8 bit signed   integers
  | Int16Rep            --         16 bit signed   integers
  | Int32Rep            --         32 bit signed   integers
  | Int64Rep	        --         64 bit signed   integers
  | Word8Rep            --          8 bit unsigned integers
  | Word16Rep           --         16 bit unsigned integers
  | Word32Rep           --         32 bit unsigned integers
  | Word64Rep	        --         64 bit unsigned integers

  | WeakPtrRep
  | ForeignObjRep	
  | BCORep

  | StablePtrRep	-- guaranteed to be represented by a pointer

  | StableNameRep	-- A stable name is a real heap object, unpointed,
			-- with one field containing an index into the
			-- stable pointer table.  It has to be a heap
			-- object so the garbage collector can track these
			-- objects and reclaim stable pointer entries.

  | ThreadIdRep		-- Really a pointer to a TSO

  | ArrayRep		-- Primitive array of Haskell pointers
  | ByteArrayRep	-- Primitive array of bytes (no Haskell pointers)

  | PrimPtrRep		-- Used for MutVars and MVars; 
			-- a pointer to a primitive object
			-- ToDo: subsumes WeakPtrRep, ThreadIdRep, 
			-- StableNameRep, ForeignObjRep, and BCORep ?

  | VoidRep		-- Occupies no space at all!
			-- (Primitive states are mapped onto this)
  deriving (Eq, Ord)
	-- Kinds are used in PrimTyCons, which need both Eq and Ord
\end{code}

These pretty much correspond to the C types declared in StgTypes.h,
with the following exceptions:

   - when an Array or ByteArray is passed to C, we again pass a pointer
     to the contents.  The actual type that is passed is StgPtr for
     ArrayRep, and StgByteArray (probably a char *) for ByteArrayRep.

These hacks are left until the final printing of the C, in
PprAbsC.lhs.

%************************************************************************
%*									*
\subsection[PrimRep-predicates]{Follow-ness, sizes, and such---on @PrimitiveKinds@}
%*									*
%************************************************************************

Whether or not the thing is a pointer that the garbage-collector
should follow. Or, to put it another (less confusing) way, whether
the object in question is a heap object. 

Depending on the outcome, this predicate determines what stack
the pointer/object possibly will have to be saved onto, and the
computation of GC liveness info.

\begin{code}
isFollowableRep :: PrimRep -> Bool

isFollowableRep PtrRep        = True
isFollowableRep ArrayRep      = True	-- all heap objects:
isFollowableRep ByteArrayRep  = True	-- 	''
isFollowableRep WeakPtrRep    = True	-- 	''
isFollowableRep ForeignObjRep = True	-- 	''
isFollowableRep StableNameRep = True    --      ''
isFollowableRep PrimPtrRep    = True    --      ''
isFollowableRep ThreadIdRep   = True	-- pointer to a TSO
isFollowableRep BCORep        = True

isFollowableRep other	      = False

separateByPtrFollowness :: (a -> PrimRep) -> [a] -> ([a], [a])

separateByPtrFollowness kind_fun things
  = sep_things kind_fun things [] []
    -- accumulating params for follow-able and don't-follow things...
  where
    sep_things kfun []     bs us = (reverse bs, reverse us)
    sep_things kfun (t:ts) bs us
      = if (isFollowableRep . kfun) t then
	    sep_things kfun ts (t:bs) us
	else
	    sep_things kfun ts bs (t:us)
\end{code}

@isFloatingRep@ is used to distinguish @Double@ and @Float@ which
cause inadvertent numeric conversions if you aren't jolly careful.
See codeGen/CgCon:cgTopRhsCon.

\begin{code}
isFloatingRep :: PrimRep -> Bool
isFloatingRep DoubleRep = True
isFloatingRep FloatRep  = True
isFloatingRep _         = False
\end{code}

\begin{code}
is64BitRep :: PrimRep -> Bool
is64BitRep Int64Rep  = True
is64BitRep Word64Rep = True
is64BitRep _         = False
\end{code}

\begin{code}
getPrimRepSize :: PrimRep -> Int
getPrimRepSize DoubleRep = dOUBLE_SIZE -- "words", of course
getPrimRepSize Word64Rep = wORD64_SIZE
getPrimRepSize Int64Rep  = iNT64_SIZE
getPrimRepSize VoidRep   = 0
getPrimRepSize _         = 1

retPrimRepSize :: Int
retPrimRepSize = getPrimRepSize RetRep

-- sizes in bytes.
-- (used in some settings to figure out how many bytes
-- we have to push onto the stack when calling external
-- entry points (e.g., stdcalling on win32)
getPrimRepSizeInBytes :: PrimRep -> Int
getPrimRepSizeInBytes CharRep       = 4
getPrimRepSizeInBytes IntRep        = wORD_SIZE
getPrimRepSizeInBytes WordRep       = wORD_SIZE
getPrimRepSizeInBytes AddrRep       = wORD_SIZE
getPrimRepSizeInBytes FloatRep      = wORD_SIZE
getPrimRepSizeInBytes DoubleRep     = dOUBLE_SIZE * wORD_SIZE
getPrimRepSizeInBytes Int8Rep       = 1
getPrimRepSizeInBytes Int16Rep      = 2
getPrimRepSizeInBytes Int32Rep      = 4
getPrimRepSizeInBytes Int64Rep      = 8
getPrimRepSizeInBytes Word8Rep      = 1
getPrimRepSizeInBytes Word16Rep     = 2
getPrimRepSizeInBytes Word32Rep     = 4
getPrimRepSizeInBytes Word64Rep     = 8
getPrimRepSizeInBytes WeakPtrRep    = wORD_SIZE
getPrimRepSizeInBytes ForeignObjRep = wORD_SIZE
getPrimRepSizeInBytes StablePtrRep  = wORD_SIZE
getPrimRepSizeInBytes StableNameRep = wORD_SIZE
getPrimRepSizeInBytes ArrayRep      = wORD_SIZE
getPrimRepSizeInBytes ByteArrayRep  = wORD_SIZE
getPrimRepSizeInBytes other         = pprPanic "getPrimRepSizeInBytes" (ppr other)


-- Sizes in bytes of things when they are array elements,
-- so that we can generate the correct indexing code
-- inside the compiler.  This is not the same as the above
-- getPrimRepSizeInBytes, the rationale behind which is
-- unclear to me.
getPrimRepArrayElemSize :: PrimRep -> Int
getPrimRepArrayElemSize PtrRep        = wORD_SIZE
getPrimRepArrayElemSize IntRep        = wORD_SIZE
getPrimRepArrayElemSize WordRep       = wORD_SIZE
getPrimRepArrayElemSize AddrRep       = wORD_SIZE
getPrimRepArrayElemSize StablePtrRep  = wORD_SIZE
getPrimRepArrayElemSize ForeignObjRep = wORD_SIZE
getPrimRepArrayElemSize Word8Rep      = 1
getPrimRepArrayElemSize Word16Rep     = 2
getPrimRepArrayElemSize Word32Rep     = 4
getPrimRepArrayElemSize Word64Rep     = 8
getPrimRepArrayElemSize Int8Rep       = 1
getPrimRepArrayElemSize Int16Rep      = 2
getPrimRepArrayElemSize Int32Rep      = 4
getPrimRepArrayElemSize Int64Rep      = 8
getPrimRepArrayElemSize FloatRep      = 4
getPrimRepArrayElemSize DoubleRep     = 8
getPrimRepArrayElemSize other         = pprPanic "getPrimRepSizeArrayElemSize" (ppr other)

\end{code}

%************************************************************************
%*									*
\subsection[PrimRep-instances]{Boring instance decls for @PrimRep@}
%*									*
%************************************************************************

\begin{code}
instance Outputable PrimRep where
    ppr kind = text (showPrimRep kind)

showPrimRep  :: PrimRep -> String
showPrimRep PtrRep	   = "P_"	-- short for StgPtr
showPrimRep CodePtrRep     = "P_"	-- DEATH to StgFunPtr! (94/02/22 WDP)
showPrimRep DataPtrRep     = "D_"
showPrimRep RetRep         = "P_"
showPrimRep CostCentreRep  = "CostCentre"
showPrimRep CharRep	   = "C_"
showPrimRep Int8Rep	   = "StgInt8"
showPrimRep Int16Rep	   = "StgInt16"
showPrimRep Int32Rep	   = "StgInt32"
showPrimRep Word8Rep	   = "StgWord8"
showPrimRep Word16Rep	   = "StgWord16"
showPrimRep Word32Rep	   = "StgWord32"
showPrimRep IntRep	   = "I_"	-- short for StgInt
showPrimRep WordRep	   = "W_"	-- short for StgWord
showPrimRep Int64Rep       = "LI_"       -- short for StgLongInt
showPrimRep Word64Rep      = "LW_"       -- short for StgLongWord
showPrimRep AddrRep	   = "StgAddr"
showPrimRep FloatRep	   = "StgFloat"
showPrimRep DoubleRep	   = "StgDouble"
showPrimRep ArrayRep	   = "P_" -- see comment below
showPrimRep PrimPtrRep	   = "P_"
showPrimRep ByteArrayRep   = "StgByteArray"
showPrimRep StablePtrRep   = "StgStablePtr"
showPrimRep StableNameRep  = "P_"
showPrimRep ThreadIdRep	   = "StgTSO*"
showPrimRep WeakPtrRep     = "P_"
showPrimRep ForeignObjRep  = "StgAddr"
showPrimRep VoidRep	   = "!!VOID_KIND!!"
showPrimRep BCORep         = "P_"  	-- not sure -- JRS 000708
\end{code}

Foreign Objects and Arrays are treated specially by the code for
_ccall_s: we pass a pointer to the contents of the object, not the
object itself.
