%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[SMRep]{Storage manager representations of closure}

This is here, rather than in ClosureInfo, just to keep nhc happy.
Other modules should access this info through ClosureInfo.

\begin{code}
#include "HsVersions.h"

module SMRep (
	SMRep(..), SMSpecRepKind(..), SMUpdateKind(..),
	getSMInfoStr, getSMInitHdrStr, getSMUpdInplaceHdrStr,
	ltSMRepHdr -- UNUSED, equivSMRepHdr
    ) where

import Outputable
import Pretty
import Util
\end{code}

%************************************************************************
%*									*
\subsubsection[SMRep-datatype]{@SMRep@---storage manager representation}
%*									*
%************************************************************************

Ways in which a closure may be represented by the storage manager;
this list slavishly follows the storage-manager interface document.

\begin{code}
data SMSpecRepKind
  = SpecRep		-- Normal Spec representation

  | ConstantRep		-- Common me up with single global copy
			-- Used for nullary constructors

  | CharLikeRep		-- Common me up with entry from global table

  | IntLikeRep		-- Common me up with entry from global table,
			-- if the intlike field is in range.

data SMUpdateKind
  = SMNormalForm	-- Normal form, no update
  | SMSingleEntry	-- Single entry thunk, non-updatable
  | SMUpdatable		-- Shared thunk, updatable

data SMRep
  = StaticRep 		-- Don't move me, Oh garbage collector!
			-- Used for all statically-allocated closures.
	Int		-- # ptr words (useful for interpreter, debugger, etc)
	Int		-- # non-ptr words

  | SpecialisedRep	-- GC routines know size etc
			-- All have same _HS = SPEC_HS and no _VHS
	SMSpecRepKind	-- Which kind of specialised representation
	Int		-- # ptr words
	Int		-- # non-ptr words
        SMUpdateKind   	-- Updatable?

  | GenericRep		-- GC routines consult sizes in info tbl
	Int		-- # ptr words
	Int		-- # non-ptr words
        SMUpdateKind	-- Updatable?

  | BigTupleRep		-- All ptrs, size in var-hdr field
			-- Used for big tuples
	Int		-- # ptr words

  | DataRep		-- All non-ptrs, size in var-hdr field
			-- Used for arbitrary-precision integers, strings
	Int		-- # non-ptr words

  | DynamicRep		-- Size and # ptrs in var-hdr field
			-- Used by RTS for partial applications

  | BlackHoleRep	-- for black hole closures

  | PhantomRep	    	-- for "phantom" closures that only exist in registers

  | MuTupleRep		-- All ptrs, size in var-hdr field
			-- Used for mutable tuples
	Int		-- # ptr words

instance Eq SMRep where
    (SpecialisedRep k1 a1 b1 _) == (SpecialisedRep k2 a2 b2 _) = (tagOf_SMSpecRepKind k1) _EQ_ (tagOf_SMSpecRepKind k2)
							       && a1 == a2 && b1 == b2
    (GenericRep a1 b1 _)      == (GenericRep a2 b2 _)	   = a1 == a2 && b1 == b2
    (BigTupleRep a1)	      == (BigTupleRep a2)	   = a1 == a2
    (MuTupleRep a1)	      == (MuTupleRep a2)	   = a1 == a2
    (DataRep a1)	      == (DataRep a2)		   = a1 == a2
    a			      == b			   = (tagOf_SMRep a) _EQ_ (tagOf_SMRep b)

{- UNUSED:
equivSMRepHdr :: SMRep -> SMRep -> Bool
a `equivSMRepHdr` b = (tagOf_SMRep a) _EQ_ (tagOf_SMRep b)
-}

ltSMRepHdr :: SMRep -> SMRep -> Bool
a `ltSMRepHdr` b = (tagOf_SMRep a) _LT_ (tagOf_SMRep b)

instance Ord SMRep where
    -- ToDo: cmp-ify?  This instance seems a bit weird (WDP 94/10)
    rep1 <= rep2 = rep1 < rep2 || rep1 == rep2
    rep1 < rep2
      =	let tag1 = tagOf_SMRep rep1
	    tag2 = tagOf_SMRep rep2
	in
	if      tag1 _LT_ tag2 then True
	else if tag1 _GT_ tag2 then False
	else {- tags equal -}	 rep1 `lt` rep2
      where
	(SpecialisedRep k1 a1 b1 _) `lt` (SpecialisedRep k2 a2 b2 _) =
		t1 _LT_ t2 || (t1 _EQ_ t2 && (a1 < a2 || (a1 == a2 && b1 < b2)))
		where t1 = tagOf_SMSpecRepKind k1
		      t2 = tagOf_SMSpecRepKind k2
	(GenericRep a1 b1 _)   	  `lt` (GenericRep a2 b2 _)	 = a1 < a2 || (a1 == a2 && b1 < b2)
	(BigTupleRep a1)          `lt` (BigTupleRep a2)	   	 = a1 < a2
	(MuTupleRep a1)           `lt` (MuTupleRep a2)	   	 = a1 < a2
	(DataRep a1)	          `lt` (DataRep a2)	   	 = a1 < a2
	a		          `lt` b			 = True

tagOf_SMSpecRepKind SpecRep	= (ILIT(1) :: FAST_INT)
tagOf_SMSpecRepKind ConstantRep	= ILIT(2)
tagOf_SMSpecRepKind CharLikeRep	= ILIT(3)
tagOf_SMSpecRepKind IntLikeRep	= ILIT(4)

tagOf_SMRep (StaticRep _ _)	     = (ILIT(1) :: FAST_INT)
tagOf_SMRep (SpecialisedRep k _ _ _) = ILIT(2)
tagOf_SMRep (GenericRep _ _ _)	     = ILIT(3)
tagOf_SMRep (BigTupleRep _)	     = ILIT(4)
tagOf_SMRep (DataRep _)		     = ILIT(5)
tagOf_SMRep DynamicRep		     = ILIT(6)
tagOf_SMRep BlackHoleRep	     = ILIT(7)
tagOf_SMRep PhantomRep	     	     = ILIT(8)
tagOf_SMRep (MuTupleRep _)	     = ILIT(9)

instance Text SMRep where
    showsPrec d rep rest
      = (case rep of
	   StaticRep _ _   	                 -> "STATIC"
	   SpecialisedRep kind _ _ SMNormalForm  -> "SPEC_N"
	   SpecialisedRep kind _ _ SMSingleEntry -> "SPEC_S"
	   SpecialisedRep kind _ _ SMUpdatable   -> "SPEC_U"
	   GenericRep _ _ SMNormalForm	         -> "GEN_N"
	   GenericRep _ _ SMSingleEntry	         -> "GEN_S"
	   GenericRep _ _ SMUpdatable	         -> "GEN_U"
	   BigTupleRep _	         -> "TUPLE"
	   DataRep	 _	         -> "DATA"
	   DynamicRep		         -> "DYN"
	   BlackHoleRep	   	         -> "BH"
	   PhantomRep	   	         -> "INREGS"
	   MuTupleRep _	            	 -> "MUTUPLE") ++ rest

instance Outputable SMRep where
    ppr sty rep = ppStr (show rep)

getSMInfoStr :: SMRep -> String
getSMInfoStr (StaticRep _ _)				= "STATIC"
getSMInfoStr (SpecialisedRep ConstantRep _ _ _)		= "CONST"
getSMInfoStr (SpecialisedRep CharLikeRep _ _ _)	    	= "CHARLIKE"
getSMInfoStr (SpecialisedRep IntLikeRep _ _ _)		= "INTLIKE"
getSMInfoStr (SpecialisedRep SpecRep _ _ SMNormalForm)	= "SPEC_N"
getSMInfoStr (SpecialisedRep SpecRep _ _ SMSingleEntry)	= "SPEC_S"
getSMInfoStr (SpecialisedRep SpecRep _ _ SMUpdatable) 	= "SPEC_U"
getSMInfoStr (GenericRep _ _ SMNormalForm) 		= "GEN_N"
getSMInfoStr (GenericRep _ _ SMSingleEntry)		= "GEN_S"
getSMInfoStr (GenericRep _ _ SMUpdatable)		= "GEN_U"
getSMInfoStr (BigTupleRep _)				= "TUPLE"
getSMInfoStr (DataRep _ )				= "DATA"
getSMInfoStr DynamicRep					= "DYN"
getSMInfoStr BlackHoleRep				= panic "getSMInfoStr.BlackHole"
getSMInfoStr PhantomRep					= "INREGS"
getSMInfoStr (MuTupleRep _)				= "MUTUPLE"

getSMInitHdrStr :: SMRep -> String
getSMInitHdrStr (SpecialisedRep IntLikeRep _ _ _)  = "SET_INTLIKE"
getSMInitHdrStr (SpecialisedRep SpecRep _ _ _) 	   = "SET_SPEC"
getSMInitHdrStr (GenericRep _ _	_)   		   = "SET_GEN"
getSMInitHdrStr (BigTupleRep _)   		   = "SET_TUPLE"
getSMInitHdrStr (DataRep _ )   			   = "SET_DATA"
getSMInitHdrStr DynamicRep	   		   = "SET_DYN"
getSMInitHdrStr BlackHoleRep	   		   = "SET_BH"
#ifdef DEBUG
getSMInitHdrStr (StaticRep _ _)			   = panic "getSMInitHdrStr.Static"
getSMInitHdrStr PhantomRep	   		   = panic "getSMInitHdrStr.Phantom"
getSMInitHdrStr (MuTupleRep _)   		   = panic "getSMInitHdrStr.Mutuple"
getSMInitHdrStr (SpecialisedRep ConstantRep _ _ _) = panic "getSMInitHdrStr.Constant"
getSMInitHdrStr (SpecialisedRep CharLikeRep _ _ _) = panic "getSMInitHdrStr.CharLike"
#endif

getSMUpdInplaceHdrStr :: SMRep -> String
getSMUpdInplaceHdrStr (SpecialisedRep ConstantRep _ _ _) = "INPLACE_UPD"
getSMUpdInplaceHdrStr (SpecialisedRep CharLikeRep _ _ _) = "INPLACE_UPD"
getSMUpdInplaceHdrStr (SpecialisedRep IntLikeRep _ _ _)	 = "INPLACE_UPD"
getSMUpdInplaceHdrStr (SpecialisedRep SpecRep _ _ _) 	 = "INPLACE_UPD"
#ifdef DEBUG
getSMUpdInplaceHdrStr (StaticRep _ _)			 = panic "getSMUpdInplaceHdrStr.Static"
getSMUpdInplaceHdrStr (GenericRep _ _ _)   		 = panic "getSMUpdInplaceHdrStr.Generic"
getSMUpdInplaceHdrStr (BigTupleRep _ )	   		 = panic "getSMUpdInplaceHdrStr.BigTuple"
getSMUpdInplaceHdrStr (DataRep _ )	   		 = panic "getSMUpdInplaceHdrStr.Data"
getSMUpdInplaceHdrStr DynamicRep	   		 = panic "getSMUpdInplaceHdrStr.Dynamic"
getSMUpdInplaceHdrStr BlackHoleRep	   		 = panic "getSMUpdInplaceHdrStr.BlackHole"
getSMUpdInplaceHdrStr PhantomRep	   		 = panic "getSMUpdInplaceHdrStr.Phantom"
getSMUpdInplaceHdrStr (MuTupleRep _ )	   		 = panic "getSMUpdInplaceHdrStr.MuTuple"
#endif
\end{code}
