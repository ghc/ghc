%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[PrimRep]{Primitive machine-level kinds of things.}

At various places in the back end, we want to be to tag things with a
``primitive kind''---i.e., the machine-manipulable implementation
types.

\begin{code}
#include "HsVersions.h"

module PrimRep (
	PrimRep(..),

	separateByPtrFollowness, isFollowableRep, isFloatingRep,
	getPrimRepSize, retPrimRepSize,
	showPrimRep,  ppPrimRep,
	guessPrimRep, decodePrimRep
    ) where

IMP_Ubiq()

import Pretty		-- pretty-printing code
import Util

#include "../../includes/GhcConstants.h"
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
  | IntRep		--	   integers (at least 32 bits)
  | WordRep		--	   ditto (but *unsigned*)
  | AddrRep		--	   addresses ("C pointers")
  | FloatRep		--	   floats
  | DoubleRep		--	   doubles

  | ForeignObjRep	-- This has to be a special kind because ccall
			-- generates special code when passing/returning
			-- one of these. [ADR]

  | StablePtrRep	-- We could replace this with IntRep but maybe
			-- there's some documentation gain from having
			-- it special? [ADR]

  | ArrayRep		-- Primitive array of Haskell pointers
  | ByteArrayRep	-- Primitive array of bytes (no Haskell pointers)

  | VoidRep		-- Occupies no space at all!
			-- (Primitive states are mapped onto this)
  deriving (Eq, Ord)
	-- Kinds are used in PrimTyCons, which need both Eq and Ord
\end{code}

%************************************************************************
%*									*
\subsection[PrimRep-predicates]{Follow-ness, sizes, and such---on @PrimitiveKinds@}
%*									*
%************************************************************************

Whether or not the thing is a pointer that the garbage-collector
should follow.

Or, to put it another (less confusing) way, whether the object in
question is a heap object.

\begin{code}
isFollowableRep :: PrimRep -> Bool

isFollowableRep PtrRep        = True
isFollowableRep ArrayRep      = True
isFollowableRep ByteArrayRep  = True
-- why is a ForeignObj followable? 4/96 SOF
--
-- A: they're followable because these objects
-- should be lugged around by the storage manager
-- (==> we need to generate code that identify them as such) -- 3/97 SOF
isFollowableRep ForeignObjRep  = True

isFollowableRep StablePtrRep  = False
-- StablePtrs aren't followable because they are just indices into a
-- table for which explicit allocation/ deallocation is required.

isFollowableRep other	    	= False

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
isFloatingRep other     = False
\end{code}

\begin{code}
getPrimRepSize :: PrimRep -> Int

getPrimRepSize DoubleRep  = DOUBLE_SIZE	-- "words", of course
--getPrimRepSize FloatRep = 1
--getPrimRepSize CharRep  = 1	-- ToDo: count in bytes?
--getPrimRepSize ArrayRep = 1	-- Listed specifically for *documentation*
--getPrimRepSize ByteArrayRep = 1
getPrimRepSize VoidRep	  = 0
getPrimRepSize other	  = 1

retPrimRepSize = getPrimRepSize RetRep
\end{code}

%************************************************************************
%*									*
\subsection[PrimRep-instances]{Boring instance decls for @PrimRep@}
%*									*
%************************************************************************

\begin{code}
instance Outputable PrimRep where
    ppr sty kind = ppStr (showPrimRep kind)

showPrimRep  :: PrimRep -> String
-- dumping PrimRep tag for unfoldings
ppPrimRep  :: PrimRep -> Pretty

guessPrimRep :: String -> PrimRep	-- a horrible "inverse" function
decodePrimRep :: Char  -> PrimRep       -- of equal nature

ppPrimRep k =
 ppChar 
  (case k of
     PtrRep        -> 'P'
     CodePtrRep    -> 'p'
     DataPtrRep    -> 'd'
     CostCentreRep -> 'c'	-- Pointer to a cost centre
     RetRep        -> 'R'
     CharRep       -> 'C'
     IntRep        -> 'I'
     WordRep       -> 'W'
     AddrRep       -> 'A'
     FloatRep      -> 'F'
     DoubleRep     -> 'D'
     ArrayRep      -> 'a'
     ByteArrayRep  -> 'b'
     StablePtrRep  -> 'S'
     ForeignObjRep -> 'f'
     VoidRep       -> 'V'
     _             -> panic "ppPrimRep")

showPrimRep PtrRep	    = "P_"	-- short for StgPtr

showPrimRep CodePtrRep    = "P_"	-- DEATH to StgFunPtr! (94/02/22 WDP)
    -- but aren't code pointers and function pointers different sizes
    -- on some machines (eg 80x86)? ADR
    -- Are you trying to ruin my life, or what? (WDP)

showPrimRep DataPtrRep    = "D_"
showPrimRep RetRep        = "StgRetAddr"
showPrimRep CostCentreRep = "CostCentre"
showPrimRep CharRep	  = "StgChar"
showPrimRep IntRep	  = "I_"	-- short for StgInt
showPrimRep WordRep	  = "W_"	-- short for StgWord
showPrimRep AddrRep	  = "StgAddr"
showPrimRep FloatRep	  = "StgFloat"
showPrimRep DoubleRep	  = "StgDouble"
showPrimRep ArrayRep	  = "StgArray" -- see comment below
showPrimRep ByteArrayRep  = "StgByteArray"
showPrimRep StablePtrRep  = "StgStablePtr"
showPrimRep ForeignObjRep  = "StgPtr" -- see comment below
showPrimRep VoidRep	  = "!!VOID_KIND!!"

decodePrimRep ch =
 case ch of
     'P' -> PtrRep        
     'p' -> CodePtrRep    
     'd' -> DataPtrRep    
     'c' -> CostCentreRep 
     'R' -> RetRep        
     'C' -> CharRep       
     'I' -> IntRep        
     'W' -> WordRep       
     'A' -> AddrRep       
     'F' -> FloatRep      
     'D' -> DoubleRep     
     'a' -> ArrayRep      
     'b' -> ByteArrayRep  
     'S' -> StablePtrRep  
     'f' -> ForeignObjRep 
     'V' -> VoidRep
     _   -> panic "decodePrimRep"

guessPrimRep "D_"	     = DataPtrRep
guessPrimRep "StgRetAddr"   = RetRep
guessPrimRep "StgChar"	     = CharRep
guessPrimRep "I_"	     = IntRep
guessPrimRep "W_"	     = WordRep
guessPrimRep "StgAddr"	     = AddrRep
guessPrimRep "StgFloat"     = FloatRep
guessPrimRep "StgDouble"    = DoubleRep
guessPrimRep "StgArray"     = ArrayRep
guessPrimRep "StgByteArray" = ByteArrayRep
guessPrimRep "StgStablePtr" = StablePtrRep
\end{code}

All local C variables of @ArrayRep@ are declared in C as type
@StgArray@.  The coercion to a more precise C type is done just before
indexing (by the relevant C primitive-op macro).

Nota Bene. There are three types associated with @ForeignObj@ (MallocPtr++): 
\begin{itemize}
\item
@StgForeignObjClosure@ is the type of the thing the prim. op @mkForeignObj@ returns.
{- old comment for MallocPtr
(This typename is hardwired into @ppr_casm_results@ in
@PprAbsC.lhs@.)
-}

\item
@StgForeignObj@ is the type of the thing we give the C world.

\item
@StgPtr@ is the type of the (pointer to the) heap object which we
pass around inside the STG machine.
\end{itemize}

It is really easy to confuse the two.  (I'm not sure this choice of
type names helps.) [ADR]
