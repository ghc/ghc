%
% (c) The GRASP Project, Glasgow University, 1992-1995
%
\section[PrimKind]{Primitive machine-level kinds of things.}

At various places in the back end, we want to be to tag things with a
``primitive kind''---i.e., the machine-manipulable implementation
types.

\begin{code}
#include "HsVersions.h"

module PrimKind (
	PrimKind(..),
	separateByPtrFollowness, isFollowableKind, isFloatingKind,
	getKindSize, retKindSize,
	getKindInfo, -- ToDo: DIE DIE DIE DIE DIE
	showPrimKind,
	guessPrimKind,

	-- and to make the interface self-sufficient...
	Id, DataCon(..), TyCon, UniType
    ) where

IMPORT_Trace

#ifdef DPH
import TyPod
#endif {- Data Parallel Haskell -}

import AbsUniType	-- we use more than I want to type in...
import Id		( Id, DataCon(..) )
import Outputable	-- class for printing, forcing
import TysPrim
import Pretty		-- pretty-printing code
import Util

#ifndef DPH
#include "../../includes/GhcConstants.h"
#else
#include "../dphsystem/imports/DphConstants.h"
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PrimKind-datatype]{The @PrimKind@ datatype}
%*									*
%************************************************************************

\begin{code}
data PrimKind
  = -- These pointer-kinds are all really the same, but we keep
    -- them separate for documentation purposes.
    PtrKind		-- Pointer to a closure; a ``word''.
  | CodePtrKind		-- Pointer to code
  | DataPtrKind		-- Pointer to data
  | RetKind 	    	-- Pointer to code or data (return vector or code pointer)
  | InfoPtrKind		-- Pointer to info table (DPH only?)
  | CostCentreKind	-- Pointer to a cost centre

  | CharKind		-- Machine characters
  | IntKind		--	   integers (at least 32 bits)
  | WordKind		--	   ditto (but *unsigned*)
  | AddrKind		--	   addresses ("C pointers")
  | FloatKind		--	   floats
  | DoubleKind		--	   doubles

  | MallocPtrKind	-- This has to be a special kind because ccall
			-- generates special code when passing/returning
			-- one of these. [ADR]

  | StablePtrKind	-- We could replace this with IntKind but maybe
			-- there's some documentation gain from having
			-- it special? [ADR]

  | ArrayKind		-- Primitive array of Haskell pointers
  | ByteArrayKind	-- Primitive array of bytes (no Haskell pointers)

  | VoidKind		-- Occupies no space at all!
			-- (Primitive states are mapped onto this)
#ifdef DPH
  | PodNKind Int PrimKind
#endif {- Data Parallel Haskell -}
  deriving (Eq, Ord)
	-- Kinds are used in PrimTyCons, which need both Eq and Ord
	-- Text is needed for derived-Text on PrimitiveOps
\end{code}

%************************************************************************
%*									*
\subsection[PrimKind-predicates]{Follow-ness, sizes, and such---on @PrimitiveKinds@}
%*									*
%************************************************************************

Whether or not the thing is a pointer that the garbage-collector
should follow.

Or, to put it another (less confusing) way, whether the object in
question is a heap object.

\begin{code}
isFollowableKind :: PrimKind -> Bool
isFollowableKind PtrKind    	= True
isFollowableKind ArrayKind  	= True
isFollowableKind ByteArrayKind 	= True
isFollowableKind MallocPtrKind  = True

isFollowableKind StablePtrKind  = False
-- StablePtrs aren't followable because they are just indices into a
-- table for which explicit allocation/ deallocation is required.

isFollowableKind other	    	= False

separateByPtrFollowness :: (a -> PrimKind) -> [a] -> ([a], [a])
separateByPtrFollowness kind_fun things
  = sep_things kind_fun things [] []
    -- accumulating params for follow-able and don't-follow things...
  where
    sep_things kfun []     bs us = (reverse bs, reverse us)
    sep_things kfun (t:ts) bs us
      = if (isFollowableKind . kfun) t then
	    sep_things kfun ts (t:bs) us
	else
	    sep_things kfun ts bs (t:us)
\end{code}

@isFloatingKind@ is used to distinguish @Double@ and @Float@ which
cause inadvertent numeric conversions if you aren't jolly careful.
See codeGen/CgCon:cgTopRhsCon.

\begin{code}
isFloatingKind :: PrimKind -> Bool
isFloatingKind DoubleKind = True
isFloatingKind FloatKind  = True
isFloatingKind other      = False
\end{code}

\begin{code}
getKindSize :: PrimKind -> Int
getKindSize DoubleKind	  = DOUBLE_SIZE	-- "words", of course
--getKindSize FloatKind	  = 1
--getKindSize CharKind	  = 1	-- ToDo: count in bytes?
--getKindSize ArrayKind	  = 1	-- Listed specifically for *documentation*
--getKindSize ByteArrayKind = 1

#ifdef DPH
getKindSize (PodNKind _ _) = panic "getKindSize: PodNKind"
#endif {- Data Parallel Haskell -}

getKindSize VoidKind	  = 0
getKindSize other	  = 1


retKindSize :: Int
retKindSize = getKindSize RetKind
\end{code}

%************************************************************************
%*									*
\subsection[PrimKind-type-fns]{@PrimitiveKinds@ and @UniTypes@}
%*									*
%************************************************************************

@PrimitiveKinds@ are used in @PrimitiveOps@, for which we often need
to reconstruct various type information.  (It's slightly more
convenient/efficient to make type info from kinds, than kinds [etc.]
from type info.)

\begin{code}
getKindInfo ::
    PrimKind -> (String,			-- tag string
		      UniType, TyCon)		-- prim type and tycon

getKindInfo CharKind   = ("Char",   charPrimTy,   charPrimTyCon)
getKindInfo IntKind    = ("Int",    intPrimTy,    intPrimTyCon)
getKindInfo WordKind   = ("Word",   wordPrimTy,   wordPrimTyCon)
getKindInfo AddrKind   = ("Addr",   addrPrimTy,   addrPrimTyCon)
getKindInfo FloatKind  = ("Float",  floatPrimTy,  floatPrimTyCon)
getKindInfo DoubleKind = ("Double", doublePrimTy, doublePrimTyCon)
#ifdef DPH
getKindInfo k@(PodNKind d kind)
  = case kind of
      PtrKind	->(no_no, no_no, no_no, no_no, no_no, no_no)
      CharKind  ->("Char.Pod"++show d, mkPodizedPodNTy d charPrimTy,
		    no_no, mkPodizedPodNTy d charTy, no_no, no_no)

      IntKind   ->("Int.Pod"++show d, mkPodizedPodNTy d intPrimTy,
		    no_no, mkPodizedPodNTy d intTy, no_no , no_no)

      FloatKind ->("Float.Pod"++show d, mkPodizedPodNTy d floatPrimTy,
		    no_no ,mkPodizedPodNTy d floatTy, no_no, no_no)

      DoubleKind->("Double.Pod"++show d, mkPodizedPodNTy d doublePrimTy,
		    no_no, mkPodizedPodNTy d doubleTy, no_no, no_no)
      AddrKind  ->("Addr.Pod"++show d, mkPodizedPodNTy d addrPrimTy,
		      no_no, no_no, no_no, no_no)
      _         -> pprPanic "Found PodNKind" (ppr PprDebug k)
   where
     no_no = panic "getKindInfo: PodNKind"

getKindInfo other = pprPanic "getKindInfo" (ppr PprDebug other)
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PrimKind-instances]{Boring instance decls for @PrimKind@}
%*									*
%************************************************************************

\begin{code}
instance Outputable PrimKind where
#ifdef DPH
    ppr sty (PodNKind d k)  = ppBesides [ppr sty k , ppStr ".POD" , ppr sty d]
#endif {- Data Parallel Haskell -}
    ppr sty kind = ppStr (showPrimKind kind)

showPrimKind  :: PrimKind -> String
guessPrimKind :: String -> PrimKind	-- a horrible "inverse" function

showPrimKind PtrKind	    = "P_"	-- short for StgPtr

showPrimKind CodePtrKind    = "P_"	-- DEATH to StgFunPtr! (94/02/22 WDP)
    -- but aren't code pointers and function pointers different sizes
    -- on some machines (eg 80x86)? ADR
    -- Are you trying to ruin my life, or what? (WDP)

showPrimKind DataPtrKind    = "D_"
showPrimKind RetKind        = "StgRetAddr"
showPrimKind InfoPtrKind    = "StgInfoPtr"
showPrimKind CostCentreKind = "CostCentre"
showPrimKind CharKind	    = "StgChar"
showPrimKind IntKind	    = "I_"	-- short for StgInt
showPrimKind WordKind	    = "W_"	-- short for StgWord
showPrimKind AddrKind	    = "StgAddr"
showPrimKind FloatKind	    = "StgFloat"
showPrimKind DoubleKind	    = "StgDouble"
showPrimKind ArrayKind	    = "StgArray" -- see comment below
showPrimKind ByteArrayKind  = "StgByteArray"
showPrimKind StablePtrKind  = "StgStablePtr"
showPrimKind MallocPtrKind  = "StgPtr" -- see comment below
showPrimKind VoidKind	    = "!!VOID_KIND!!"

guessPrimKind "D_"	     = DataPtrKind
guessPrimKind "StgRetAddr"   = RetKind
guessPrimKind "StgInfoPtr"   = InfoPtrKind
guessPrimKind "StgChar"	     = CharKind
guessPrimKind "I_"	     = IntKind
guessPrimKind "W_"	     = WordKind
guessPrimKind "StgAddr"	     = AddrKind
guessPrimKind "StgFloat"     = FloatKind
guessPrimKind "StgDouble"    = DoubleKind
guessPrimKind "StgArray"     = ArrayKind
guessPrimKind "StgByteArray" = ByteArrayKind
guessPrimKind "StgStablePtr" = StablePtrKind
\end{code}

All local C variables of @ArrayKind@ are declared in C as type
@StgArray@.  The coercion to a more precise C type is done just before
indexing (by the relevant C primitive-op macro).

Nota Bene. There are three types associated with Malloc Pointers: 
\begin{itemize}
\item
@StgMallocClosure@ is the type of the thing the C world gives us.
(This typename is hardwired into @ppr_casm_results@ in
@PprAbsC.lhs@.)

\item
@StgMallocPtr@ is the type of the thing we give the C world.

\item
@StgPtr@ is the type of the (pointer to the) heap object which we
pass around inside the STG machine.
\end{itemize}

It is really easy to confuse the two.  (I'm not sure this choice of
type names helps.) [ADR]
