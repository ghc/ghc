%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[PrimRep]{Primitive machine-level kinds of things.}

At various places in the back end, we want to be to tag things with a
``primitive kind''---i.e., the machine-manipulable implementation
types.

\begin{code}
module PrimRep (
	PrimRep(..),
	separateByPtrFollowness,
	isFollowableRep,
	isFloatingRep,
	isNonPtrRep,	 
	is64BitRep,
	getPrimRepSize,
	getPrimRepSizeInBytes,
	retPrimRepSize,

	ArgRep(..), primRepToArgRep,
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

These pretty much correspond to the C types declared in StgTypes.h.

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

  | StablePtrRep	-- guaranteed to be represented by a pointer

  | VoidRep		-- Occupies no space at all!
			-- (Primitive states are mapped onto this)
  deriving (Eq, Ord)
\end{code}


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
isFollowableRep :: PrimRep -> Bool  -- True <=> points to a heap object
isFollowableRep PtrRep 	      = True
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

Identify anything which is one word large and not a pointer.

\begin{code}
isNonPtrRep :: PrimRep -> Bool
isNonPtrRep PtrRep  = False
isNonPtrRep VoidRep = False
isNonPtrRep r       = not (isFloatingRep r) && not (is64BitRep r)
\end{code}

\begin{code}
is64BitRep :: PrimRep -> Bool
is64BitRep Int64Rep  = True
is64BitRep Word64Rep = True
is64BitRep _         = False

-- Size in words.

getPrimRepSize :: PrimRep -> Int
getPrimRepSize DoubleRep = dOUBLE_SIZE
getPrimRepSize Word64Rep = wORD64_SIZE
getPrimRepSize Int64Rep  = iNT64_SIZE
getPrimRepSize VoidRep   = 0
getPrimRepSize _         = 1

retPrimRepSize :: Int
retPrimRepSize = getPrimRepSize RetRep

-- Sizes in bytes.  (used in some settings to figure out how many
-- bytes we have to push onto the stack when calling external entry
-- points (e.g., stdcalling on win32)

-- Note: the "size in bytes" is also the scaling factor used when we
-- have an array of these things.  For example, a ByteArray# of
-- Int16Rep will use a scaling factor of 2 when accessing the
-- elements.

getPrimRepSizeInBytes :: PrimRep -> Int
getPrimRepSizeInBytes PtrRep        = wORD_SIZE
getPrimRepSizeInBytes CodePtrRep    = wORD_SIZE
getPrimRepSizeInBytes DataPtrRep    = wORD_SIZE
getPrimRepSizeInBytes RetRep        = wORD_SIZE
getPrimRepSizeInBytes CostCentreRep = wORD_SIZE
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
getPrimRepSizeInBytes StablePtrRep  = wORD_SIZE
getPrimRepSizeInBytes other         = pprPanic "getPrimRepSizeInBytes" (ppr other)
\end{code}

%************************************************************************
%*									*
\subsection{ArgReps}
%*									*
%************************************************************************

An ArgRep is similar to a PrimRep, except that it is slightly
narrower.  It corresponds to the distinctions we make between
different type of function arguments for the purposes of a function's
calling convention.  These reps are used to decide which of the RTS's
generic apply functions to call when applying an unknown function.

All 64-bit PrimReps map to the same ArgRep, because they're passed in
the same register, but a PtrRep is still different from an IntRep
(RepP vs. RepN respectively) because the function's entry convention
has to take into account the pointer-hood of arguments for the
purposes of describing the stack on entry to the garbage collector.

\begin{code}
data ArgRep = RepV | RepP | RepN | RepF | RepD | RepL

primRepToArgRep VoidRep   = RepV
primRepToArgRep FloatRep  = RepF
primRepToArgRep DoubleRep = RepD
primRepToArgRep r
   | isFollowableRep r     = RepP
   | is64BitRep r          = RepL
   | otherwise             = ASSERT(getPrimRepSize r == 1) RepN
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
showPrimRep StablePtrRep   = "StgStablePtr"
showPrimRep VoidRep	   = "!!VOID_KIND!!"
\end{code}


