%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrimOp]{Primitive operations (machine-level)}

\begin{code}
#include "HsVersions.h"

module PrimOp (
	PrimOp(..), allThePrimOps,
	tagOf_PrimOp, -- ToDo: rm
	primOp_str,   -- sigh
	primOpType, isCompareOp,
	commutableOp,

	PrimOpResultInfo(..),
	getPrimOpResultInfo,

	primOpCanTriggerGC, primOpNeedsWrapper,
	primOpOkForSpeculation, primOpIsCheap,
	fragilePrimOp,
	HeapRequirement(..), primOpHeapReq,
	StackRequirement(..), primOpStackRequired,	

       -- export for the Native Code Generator
	primOpInfo, -- needed for primOpNameInfo
	PrimOpInfo(..),

	pprPrimOp, showPrimOp
    ) where

IMP_Ubiq(){-uitous-}

import PrimRep		-- most of it
import TysPrim
import TysWiredIn

import CStrings		( identToC )
import CgCompInfo   	( mIN_MP_INT_SIZE, mP_STRUCT_SIZE )
import HeapOffs		( addOff, intOff, totHdrSize, HeapOffset )
import PprStyle		( codeStyle{-, PprStyle(..) ToDo:rm-} )
import PprType		( pprParendGenType, GenTyVar{-instance Outputable-} )
import Pretty
import SMRep	    	( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import TyCon		( TyCon{-instances-} )
import Type		( getAppDataTyConExpandingDicts, maybeAppDataTyConExpandingDicts,
			  mkForAllTys, mkFunTy, mkFunTys, applyTyCon, typePrimRep
			)
import TyVar		( alphaTyVar, betaTyVar, gammaTyVar, GenTyVar{-instance Eq-} )
import Unique		( Unique{-instance Eq-} )
import Util		( panic#, assoc, panic{-ToDo:rm-} )
\end{code}

%************************************************************************
%*									*
\subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}
%*									*
%************************************************************************

These are in \tr{state-interface.verb} order.

\begin{code}
data PrimOp
    -- dig the FORTRAN/C influence on the names...

    -- comparisons:

    = CharGtOp   | CharGeOp   | CharEqOp   | CharNeOp   | CharLtOp   | CharLeOp
    | IntGtOp    | IntGeOp    | IntEqOp    | IntNeOp	| IntLtOp    | IntLeOp
    | WordGtOp   | WordGeOp   | WordEqOp   | WordNeOp	| WordLtOp   | WordLeOp
    | AddrGtOp   | AddrGeOp   | AddrEqOp   | AddrNeOp	| AddrLtOp   | AddrLeOp
    | FloatGtOp	 | FloatGeOp  | FloatEqOp  | FloatNeOp	| FloatLtOp  | FloatLeOp
    | DoubleGtOp | DoubleGeOp | DoubleEqOp | DoubleNeOp | DoubleLtOp | DoubleLeOp

    -- Char#-related ops:
    | OrdOp | ChrOp

    -- Int#-related ops:
    -- IntAbsOp unused?? ADR
    | IntAddOp | IntSubOp | IntMulOp | IntQuotOp
    | IntRemOp | IntNegOp | IntAbsOp

    -- Word#-related ops:
    | AndOp  | OrOp   | NotOp
    | SllOp  | SraOp  | SrlOp  -- shift {left,right} {arithmetic,logical}
    | ISllOp | ISraOp | ISrlOp -- equivs on Int#s
    | Int2WordOp | Word2IntOp -- casts

    -- Addr#-related ops:
    | Int2AddrOp | Addr2IntOp -- casts

    -- Float#-related ops:
    | FloatAddOp | FloatSubOp | FloatMulOp | FloatDivOp | FloatNegOp
    | Float2IntOp | Int2FloatOp

    | FloatExpOp   | FloatLogOp	  | FloatSqrtOp
    | FloatSinOp   | FloatCosOp	  | FloatTanOp
    | FloatAsinOp  | FloatAcosOp  | FloatAtanOp
    | FloatSinhOp  | FloatCoshOp  | FloatTanhOp
    -- not all machines have these available conveniently:
    -- | FloatAsinhOp | FloatAcoshOp | FloatAtanhOp
    | FloatPowerOp -- ** op

    -- Double#-related ops:
    | DoubleAddOp | DoubleSubOp | DoubleMulOp | DoubleDivOp | DoubleNegOp
    | Double2IntOp | Int2DoubleOp
    | Double2FloatOp | Float2DoubleOp

    | DoubleExpOp   | DoubleLogOp   | DoubleSqrtOp
    | DoubleSinOp   | DoubleCosOp   | DoubleTanOp
    | DoubleAsinOp  | DoubleAcosOp  | DoubleAtanOp
    | DoubleSinhOp  | DoubleCoshOp  | DoubleTanhOp
    -- not all machines have these available conveniently:
    -- | DoubleAsinhOp | DoubleAcoshOp | DoubleAtanhOp
    | DoublePowerOp -- ** op

    -- Integer (and related...) ops:
    -- slightly weird -- to match GMP package.
    | IntegerAddOp | IntegerSubOp | IntegerMulOp
    | IntegerQuotRemOp | IntegerDivModOp | IntegerNegOp

    | IntegerCmpOp

    | Integer2IntOp  | Int2IntegerOp
    | Word2IntegerOp
    | Addr2IntegerOp -- "Addr" is *always* a literal string
    -- ?? gcd, etc?

    | FloatEncodeOp  | FloatDecodeOp
    | DoubleEncodeOp | DoubleDecodeOp

    -- primitive ops for primitive arrays

    | NewArrayOp
    | NewByteArrayOp PrimRep

    | SameMutableArrayOp
    | SameMutableByteArrayOp

    | ReadArrayOp | WriteArrayOp | IndexArrayOp -- for arrays of Haskell ptrs

    | ReadByteArrayOp	PrimRep
    | WriteByteArrayOp	PrimRep
    | IndexByteArrayOp	PrimRep
    | IndexOffAddrOp	PrimRep
	-- PrimRep can be one of {Char,Int,Addr,Float,Double}Kind.
	-- This is just a cheesy encoding of a bunch of ops.
	-- Note that ForeignObjRep is not included -- the only way of
	-- creating a ForeignObj is with a ccall or casm.

    | UnsafeFreezeArrayOp | UnsafeFreezeByteArrayOp

    | NewSynchVarOp -- for MVars and IVars
    | TakeMVarOp | PutMVarOp
    | ReadIVarOp | WriteIVarOp

    | MakeForeignObjOp -- foreign objects (malloc pointers or any old URL)
    | MakeStablePtrOp | DeRefStablePtrOp
\end{code}

A special ``trap-door'' to use in making calls direct to C functions:
\begin{code}
    | CCallOp	FAST_STRING	-- An "unboxed" ccall# to this named function
		Bool		-- True <=> really a "casm"
		Bool		-- True <=> might invoke Haskell GC
		[Type]	-- Unboxed argument; the state-token
				-- argument will have been put *first*
		Type		-- Return type; one of the "StateAnd<blah>#" types

    -- (... to be continued ... )
\end{code}

The ``type'' of @CCallOp foo [t1, ... tm] r@ is @t1 -> ... tm -> r@.
(See @primOpInfo@ for details.)

Note: that first arg and part of the result should be the system state
token (which we carry around to fool over-zealous optimisers) but
which isn't actually passed.

For example, we represent
\begin{pseudocode}
((ccall# foo [StablePtr# a, Int] Float) sp# i#) :: (Float, IoWorld)
\end{pseudocode}
by
\begin{pseudocode}
Case
  ( Prim
      (CCallOp "foo" [Universe#, StablePtr# a, Int#] FloatPrimAndUniverse False)
       -- :: Universe# -> StablePtr# a -> Int# -> FloatPrimAndUniverse
      []
      [w#, sp# i#]
  )
  (AlgAlts [ ( FloatPrimAndIoWorld,
		 [f#, w#],
		 Con (TupleCon 2) [Float, IoWorld] [F# f#, World w#]
	       ) ]
	     NoDefault
  )
\end{pseudocode}

Nota Bene: there are some people who find the empty list of types in
the @Prim@ somewhat puzzling and would represent the above by
\begin{pseudocode}
Case
  ( Prim
      (CCallOp "foo" [alpha1, alpha2, alpha3] alpha4 False)
       -- :: /\ alpha1, alpha2 alpha3, alpha4.
       --       alpha1 -> alpha2 -> alpha3 -> alpha4
      [Universe#, StablePtr# a, Int#, FloatPrimAndIoWorld]
      [w#, sp# i#]
  )
  (AlgAlts [ ( FloatPrimAndIoWorld,
		 [f#, w#],
		 Con (TupleCon 2) [Float, IoWorld] [F# f#, World w#]
	       ) ]
	     NoDefault
  )
\end{pseudocode}

But, this is a completely different way of using @CCallOp@.  The most
major changes required if we switch to this are in @primOpInfo@, and
the desugarer. The major difficulty is in moving the HeapRequirement
stuff somewhere appropriate.  (The advantage is that we could simplify
@CCallOp@ and record just the number of arguments with corresponding
simplifications in reading pragma unfoldings, the simplifier,
instantiation (etc) of core expressions, ... .  Maybe we should think
about using it this way?? ADR)

\begin{code}
    -- (... continued from above ... )

    -- one to support "errorIO" (and, thereby, "error")
    | ErrorIOPrimOp

    -- Operation to test two closure addresses for equality (yes really!)
    -- BLAME ALASTAIR REID FOR THIS!  THE REST OF US ARE INNOCENT!
    | ReallyUnsafePtrEqualityOp

    -- three for parallel stuff
    | SeqOp
    | ParOp
    | ForkOp

    -- three for concurrency
    | DelayOp
    | WaitReadOp
    | WaitWriteOp

    | ParGlobalOp	-- named global par
    | ParLocalOp	-- named local par
    | ParAtOp		-- specifies destination of local par
    | ParAtAbsOp	-- specifies destination of local par (abs processor)
    | ParAtRelOp	-- specifies destination of local par (rel processor)
    | ParAtForNowOp	-- specifies initial destination of global par
    | CopyableOp	-- marks copyable code
    | NoFollowOp	-- marks non-followup expression
\end{code}

Deriving Ix is what we really want! ToDo
(Chk around before deleting...)
\begin{code}
tagOf_PrimOp CharGtOp			= (ILIT(1) :: FAST_INT)
tagOf_PrimOp CharGeOp			= ILIT(  2)
tagOf_PrimOp CharEqOp			= ILIT(  3)
tagOf_PrimOp CharNeOp			= ILIT(  4)
tagOf_PrimOp CharLtOp			= ILIT(  5)
tagOf_PrimOp CharLeOp			= ILIT(  6)
tagOf_PrimOp IntGtOp			= ILIT(  7)
tagOf_PrimOp IntGeOp			= ILIT(  8)
tagOf_PrimOp IntEqOp			= ILIT(  9)
tagOf_PrimOp IntNeOp			= ILIT( 10)
tagOf_PrimOp IntLtOp			= ILIT( 11)
tagOf_PrimOp IntLeOp			= ILIT( 12)
tagOf_PrimOp WordGtOp			= ILIT( 13)
tagOf_PrimOp WordGeOp			= ILIT( 14)
tagOf_PrimOp WordEqOp			= ILIT( 15)
tagOf_PrimOp WordNeOp			= ILIT( 16)
tagOf_PrimOp WordLtOp			= ILIT( 17)
tagOf_PrimOp WordLeOp			= ILIT( 18)
tagOf_PrimOp AddrGtOp			= ILIT( 19)
tagOf_PrimOp AddrGeOp			= ILIT( 20)
tagOf_PrimOp AddrEqOp			= ILIT( 21)
tagOf_PrimOp AddrNeOp			= ILIT( 22)
tagOf_PrimOp AddrLtOp			= ILIT( 23)
tagOf_PrimOp AddrLeOp			= ILIT( 24)
tagOf_PrimOp FloatGtOp			= ILIT( 25)
tagOf_PrimOp FloatGeOp			= ILIT( 26)
tagOf_PrimOp FloatEqOp			= ILIT( 27)
tagOf_PrimOp FloatNeOp			= ILIT( 28)
tagOf_PrimOp FloatLtOp			= ILIT( 29)
tagOf_PrimOp FloatLeOp			= ILIT( 30)
tagOf_PrimOp DoubleGtOp			= ILIT( 31)
tagOf_PrimOp DoubleGeOp			= ILIT( 32)
tagOf_PrimOp DoubleEqOp			= ILIT( 33)
tagOf_PrimOp DoubleNeOp			= ILIT( 34)
tagOf_PrimOp DoubleLtOp			= ILIT( 35)
tagOf_PrimOp DoubleLeOp			= ILIT( 36)
tagOf_PrimOp OrdOp			= ILIT( 37)
tagOf_PrimOp ChrOp			= ILIT( 38)
tagOf_PrimOp IntAddOp			= ILIT( 39)
tagOf_PrimOp IntSubOp			= ILIT( 40)
tagOf_PrimOp IntMulOp			= ILIT( 41)
tagOf_PrimOp IntQuotOp			= ILIT( 42)
tagOf_PrimOp IntRemOp			= ILIT( 44)
tagOf_PrimOp IntNegOp			= ILIT( 45)
tagOf_PrimOp IntAbsOp			= ILIT( 46)
tagOf_PrimOp AndOp			= ILIT( 47)
tagOf_PrimOp OrOp			= ILIT( 48)
tagOf_PrimOp NotOp			= ILIT( 49)
tagOf_PrimOp SllOp			= ILIT( 50)
tagOf_PrimOp SraOp			= ILIT( 51)
tagOf_PrimOp SrlOp			= ILIT( 52)
tagOf_PrimOp ISllOp			= ILIT( 53)
tagOf_PrimOp ISraOp			= ILIT( 54)
tagOf_PrimOp ISrlOp			= ILIT( 55)
tagOf_PrimOp Int2WordOp			= ILIT( 56)
tagOf_PrimOp Word2IntOp			= ILIT( 57)
tagOf_PrimOp Int2AddrOp			= ILIT( 58)
tagOf_PrimOp Addr2IntOp			= ILIT( 59)
tagOf_PrimOp FloatAddOp			= ILIT( 60)
tagOf_PrimOp FloatSubOp			= ILIT( 61)
tagOf_PrimOp FloatMulOp			= ILIT( 62)
tagOf_PrimOp FloatDivOp			= ILIT( 63)
tagOf_PrimOp FloatNegOp			= ILIT( 64)
tagOf_PrimOp Float2IntOp		= ILIT( 65)
tagOf_PrimOp Int2FloatOp		= ILIT( 66)
tagOf_PrimOp FloatExpOp			= ILIT( 67)
tagOf_PrimOp FloatLogOp			= ILIT( 68)
tagOf_PrimOp FloatSqrtOp		= ILIT( 69)
tagOf_PrimOp FloatSinOp			= ILIT( 70)
tagOf_PrimOp FloatCosOp			= ILIT( 71)
tagOf_PrimOp FloatTanOp			= ILIT( 72)
tagOf_PrimOp FloatAsinOp		= ILIT( 73)
tagOf_PrimOp FloatAcosOp		= ILIT( 74)
tagOf_PrimOp FloatAtanOp		= ILIT( 75)
tagOf_PrimOp FloatSinhOp		= ILIT( 76)
tagOf_PrimOp FloatCoshOp		= ILIT( 77)
tagOf_PrimOp FloatTanhOp		= ILIT( 78)
tagOf_PrimOp FloatPowerOp		= ILIT( 79)
tagOf_PrimOp DoubleAddOp		= ILIT( 80)
tagOf_PrimOp DoubleSubOp		= ILIT( 81)
tagOf_PrimOp DoubleMulOp		= ILIT( 82)
tagOf_PrimOp DoubleDivOp		= ILIT( 83)
tagOf_PrimOp DoubleNegOp		= ILIT( 84)
tagOf_PrimOp Double2IntOp		= ILIT( 85)
tagOf_PrimOp Int2DoubleOp		= ILIT( 86)
tagOf_PrimOp Double2FloatOp		= ILIT( 87)
tagOf_PrimOp Float2DoubleOp		= ILIT( 88)
tagOf_PrimOp DoubleExpOp		= ILIT( 89)
tagOf_PrimOp DoubleLogOp		= ILIT( 90)
tagOf_PrimOp DoubleSqrtOp		= ILIT( 91)
tagOf_PrimOp DoubleSinOp		= ILIT( 92)
tagOf_PrimOp DoubleCosOp		= ILIT( 93)
tagOf_PrimOp DoubleTanOp		= ILIT( 94)
tagOf_PrimOp DoubleAsinOp		= ILIT( 95)
tagOf_PrimOp DoubleAcosOp		= ILIT( 96)
tagOf_PrimOp DoubleAtanOp		= ILIT( 97)
tagOf_PrimOp DoubleSinhOp		= ILIT( 98)
tagOf_PrimOp DoubleCoshOp		= ILIT( 99)
tagOf_PrimOp DoubleTanhOp		= ILIT(100)
tagOf_PrimOp DoublePowerOp		= ILIT(101)
tagOf_PrimOp IntegerAddOp		= ILIT(102)
tagOf_PrimOp IntegerSubOp		= ILIT(103)
tagOf_PrimOp IntegerMulOp		= ILIT(104)
tagOf_PrimOp IntegerQuotRemOp		= ILIT(105)
tagOf_PrimOp IntegerDivModOp		= ILIT(106)
tagOf_PrimOp IntegerNegOp		= ILIT(107)
tagOf_PrimOp IntegerCmpOp		= ILIT(108)
tagOf_PrimOp Integer2IntOp		= ILIT(109)
tagOf_PrimOp Int2IntegerOp		= ILIT(110)
tagOf_PrimOp Word2IntegerOp		= ILIT(111)
tagOf_PrimOp Addr2IntegerOp		= ILIT(112)
tagOf_PrimOp FloatEncodeOp		= ILIT(113)
tagOf_PrimOp FloatDecodeOp		= ILIT(114)
tagOf_PrimOp DoubleEncodeOp		= ILIT(115)
tagOf_PrimOp DoubleDecodeOp		= ILIT(116)
tagOf_PrimOp NewArrayOp			= ILIT(117)
tagOf_PrimOp (NewByteArrayOp CharRep)	= ILIT(118)
tagOf_PrimOp (NewByteArrayOp IntRep)	= ILIT(119)
tagOf_PrimOp (NewByteArrayOp AddrRep)	= ILIT(120)
tagOf_PrimOp (NewByteArrayOp FloatRep)	= ILIT(121)
tagOf_PrimOp (NewByteArrayOp DoubleRep)= ILIT(122)
tagOf_PrimOp SameMutableArrayOp		= ILIT(123)
tagOf_PrimOp SameMutableByteArrayOp	= ILIT(124)
tagOf_PrimOp ReadArrayOp		= ILIT(125)
tagOf_PrimOp WriteArrayOp		= ILIT(126)
tagOf_PrimOp IndexArrayOp		= ILIT(127)
tagOf_PrimOp (ReadByteArrayOp CharRep)	    = ILIT(128)
tagOf_PrimOp (ReadByteArrayOp IntRep)	    = ILIT(129)
tagOf_PrimOp (ReadByteArrayOp AddrRep)	    = ILIT(130)
tagOf_PrimOp (ReadByteArrayOp FloatRep)    = ILIT(131)
tagOf_PrimOp (ReadByteArrayOp DoubleRep)   = ILIT(132)
tagOf_PrimOp (WriteByteArrayOp CharRep)    = ILIT(133)
tagOf_PrimOp (WriteByteArrayOp IntRep)	    = ILIT(134)
tagOf_PrimOp (WriteByteArrayOp AddrRep)    = ILIT(135)
tagOf_PrimOp (WriteByteArrayOp FloatRep)   = ILIT(136)
tagOf_PrimOp (WriteByteArrayOp DoubleRep)  = ILIT(137)
tagOf_PrimOp (IndexByteArrayOp CharRep)    = ILIT(138)
tagOf_PrimOp (IndexByteArrayOp IntRep)	    = ILIT(139)
tagOf_PrimOp (IndexByteArrayOp AddrRep)    = ILIT(140)
tagOf_PrimOp (IndexByteArrayOp FloatRep)   = ILIT(141)
tagOf_PrimOp (IndexByteArrayOp DoubleRep)  = ILIT(142)
tagOf_PrimOp (IndexOffAddrOp CharRep)	    = ILIT(143)
tagOf_PrimOp (IndexOffAddrOp IntRep)	    = ILIT(144)
tagOf_PrimOp (IndexOffAddrOp AddrRep)	    = ILIT(145)
tagOf_PrimOp (IndexOffAddrOp FloatRep)	    = ILIT(146)
tagOf_PrimOp (IndexOffAddrOp DoubleRep)    = ILIT(147)
tagOf_PrimOp UnsafeFreezeArrayOp	    = ILIT(148)
tagOf_PrimOp UnsafeFreezeByteArrayOp	    = ILIT(149)
tagOf_PrimOp NewSynchVarOp		    = ILIT(150)
tagOf_PrimOp TakeMVarOp		    	    = ILIT(151)
tagOf_PrimOp PutMVarOp		    	    = ILIT(152)
tagOf_PrimOp ReadIVarOp		    	    = ILIT(153)
tagOf_PrimOp WriteIVarOp		    = ILIT(154)
tagOf_PrimOp MakeForeignObjOp		    = ILIT(155)
tagOf_PrimOp MakeStablePtrOp		    = ILIT(156)
tagOf_PrimOp DeRefStablePtrOp		    = ILIT(157)
tagOf_PrimOp (CCallOp _ _ _ _ _)	    = ILIT(158)
tagOf_PrimOp ErrorIOPrimOp		    = ILIT(159)
tagOf_PrimOp ReallyUnsafePtrEqualityOp	    = ILIT(160)
tagOf_PrimOp SeqOp			    = ILIT(161)
tagOf_PrimOp ParOp			    = ILIT(162)
tagOf_PrimOp ForkOp			    = ILIT(163)
tagOf_PrimOp DelayOp			    = ILIT(164)
tagOf_PrimOp WaitReadOp			    = ILIT(165)
tagOf_PrimOp WaitWriteOp		    = ILIT(166)

tagOf_PrimOp ParGlobalOp		    = ILIT(167)
tagOf_PrimOp ParLocalOp			    = ILIT(168)
tagOf_PrimOp ParAtOp			    = ILIT(169)
tagOf_PrimOp ParAtAbsOp			    = ILIT(170)
tagOf_PrimOp ParAtRelOp			    = ILIT(171)
tagOf_PrimOp ParAtForNowOp		    = ILIT(172)
tagOf_PrimOp CopyableOp			    = ILIT(173)
tagOf_PrimOp NoFollowOp			    = ILIT(174)

tagOf_PrimOp _ = panic# "tagOf_PrimOp: pattern-match"

instance Eq PrimOp where
    op == op2 = tagOf_PrimOp op _EQ_ tagOf_PrimOp op2
\end{code}

An @Enum@-derived list would be better; meanwhile... (ToDo)
\begin{code}
allThePrimOps
  = [	CharGtOp,
	CharGeOp,
	CharEqOp,
	CharNeOp,
	CharLtOp,
	CharLeOp,
	IntGtOp,
	IntGeOp,
	IntEqOp,
	IntNeOp,
	IntLtOp,
	IntLeOp,
	WordGtOp,
	WordGeOp,
	WordEqOp,
	WordNeOp,
	WordLtOp,
	WordLeOp,
	AddrGtOp,
	AddrGeOp,
	AddrEqOp,
	AddrNeOp,
	AddrLtOp,
	AddrLeOp,
	FloatGtOp,
	FloatGeOp,
	FloatEqOp,
	FloatNeOp,
	FloatLtOp,
	FloatLeOp,
	DoubleGtOp,
	DoubleGeOp,
	DoubleEqOp,
	DoubleNeOp,
	DoubleLtOp,
	DoubleLeOp,
	OrdOp,
	ChrOp,
	IntAddOp,
	IntSubOp,
	IntMulOp,
	IntQuotOp,
	IntRemOp,
	IntNegOp,
	AndOp,
	OrOp,
	NotOp,
    	SllOp,
    	SraOp,
    	SrlOp,
    	ISllOp,
    	ISraOp,
    	ISrlOp,
	Int2WordOp,
	Word2IntOp,
	Int2AddrOp,
	Addr2IntOp,
	FloatAddOp,
	FloatSubOp,
	FloatMulOp,
	FloatDivOp,
	FloatNegOp,
	Float2IntOp,
	Int2FloatOp,
	FloatExpOp,
	FloatLogOp,
	FloatSqrtOp,
	FloatSinOp,
	FloatCosOp,
	FloatTanOp,
	FloatAsinOp,
	FloatAcosOp,
	FloatAtanOp,
	FloatSinhOp,
	FloatCoshOp,
	FloatTanhOp,
	FloatPowerOp,
	DoubleAddOp,
	DoubleSubOp,
	DoubleMulOp,
	DoubleDivOp,
	DoubleNegOp,
	Double2IntOp,
	Int2DoubleOp,
	Double2FloatOp,
	Float2DoubleOp,
	DoubleExpOp,
	DoubleLogOp,
	DoubleSqrtOp,
	DoubleSinOp,
	DoubleCosOp,
	DoubleTanOp,
	DoubleAsinOp,
	DoubleAcosOp,
	DoubleAtanOp,
	DoubleSinhOp,
	DoubleCoshOp,
	DoubleTanhOp,
	DoublePowerOp,
	IntegerAddOp,
	IntegerSubOp,
	IntegerMulOp,
	IntegerQuotRemOp,
	IntegerDivModOp,
	IntegerNegOp,
	IntegerCmpOp,
	Integer2IntOp,
	Int2IntegerOp,
	Word2IntegerOp,
	Addr2IntegerOp,
	FloatEncodeOp,
	FloatDecodeOp,
	DoubleEncodeOp,
	DoubleDecodeOp,
	NewArrayOp,
	NewByteArrayOp CharRep,
	NewByteArrayOp IntRep,
	NewByteArrayOp AddrRep,
	NewByteArrayOp FloatRep,
	NewByteArrayOp DoubleRep,
	SameMutableArrayOp,
	SameMutableByteArrayOp,
	ReadArrayOp,
	WriteArrayOp,
	IndexArrayOp,
	ReadByteArrayOp CharRep,
	ReadByteArrayOp IntRep,
	ReadByteArrayOp AddrRep,
	ReadByteArrayOp FloatRep,
	ReadByteArrayOp DoubleRep,
	WriteByteArrayOp CharRep,
	WriteByteArrayOp IntRep,
	WriteByteArrayOp AddrRep,
	WriteByteArrayOp FloatRep,
	WriteByteArrayOp DoubleRep,
	IndexByteArrayOp CharRep,
	IndexByteArrayOp IntRep,
	IndexByteArrayOp AddrRep,
	IndexByteArrayOp FloatRep,
	IndexByteArrayOp DoubleRep,
	IndexOffAddrOp CharRep,
	IndexOffAddrOp IntRep,
	IndexOffAddrOp AddrRep,
	IndexOffAddrOp FloatRep,
	IndexOffAddrOp DoubleRep,
	UnsafeFreezeArrayOp,
	UnsafeFreezeByteArrayOp,
    	NewSynchVarOp,
	ReadArrayOp,
	TakeMVarOp,
	PutMVarOp,
	ReadIVarOp,
	WriteIVarOp,
	MakeForeignObjOp,
	MakeStablePtrOp,
	DeRefStablePtrOp,
	ReallyUnsafePtrEqualityOp,
	ErrorIOPrimOp,
	ParGlobalOp,
	ParLocalOp,
	ParAtOp,
	ParAtAbsOp,
	ParAtRelOp,
	ParAtForNowOp,
	CopyableOp,
	NoFollowOp,
	SeqOp,
    	ParOp,
    	ForkOp,
	DelayOp,
	WaitReadOp,
	WaitWriteOp
    ]
\end{code}

%************************************************************************
%*									*
\subsection[PrimOp-info]{The essential info about each @PrimOp@}
%*									*
%************************************************************************

The @String@ in the @PrimOpInfos@ is the ``base name'' by which the user may
refer to the primitive operation.  The conventional \tr{#}-for-
unboxed ops is added on later.

The reason for the funny characters in the names is so we do not
interfere with the programmer's Haskell name spaces.

We use @PrimKinds@ for the ``type'' information, because they're
(slightly) more convenient to use than @TyCons@.
\begin{code}
data PrimOpInfo
  = Dyadic	FAST_STRING	-- string :: T -> T -> T
		Type
  | Monadic	FAST_STRING	-- string :: T -> T
		Type
  | Compare	FAST_STRING	-- string :: T -> T -> Bool
		Type
  | Coercing	FAST_STRING	-- string :: T1 -> T2
		Type
		Type

  | PrimResult	FAST_STRING
		[TyVar] [Type] TyCon PrimRep [Type]
		-- "PrimResult tvs [t1,..,tn] D# kind [s1,..,sm]"
		-- has type Forall tvs. t1 -> ... -> tn -> (D# s1 ... sm)
		-- D# is a primitive type constructor.
		-- (the kind is the same info as D#, in another convenient form)

  | AlgResult	FAST_STRING
		[TyVar] [Type] TyCon [Type]
		-- "AlgResult tvs [t1,..,tn] T [s1,..,sm]"
		-- has type Forall tvs. t1 -> ... -> tn -> (T s1 ... sm)

-- ToDo: Specialised calls to PrimOps are prohibited but may be desirable
\end{code}

Utility bits:
\begin{code}
one_Integer_ty = [intPrimTy, intPrimTy, byteArrayPrimTy]
two_Integer_tys
  = [intPrimTy, intPrimTy, byteArrayPrimTy, -- first Integer pieces
     intPrimTy, intPrimTy, byteArrayPrimTy] -- second '' pieces
an_Integer_and_Int_tys
  = [intPrimTy, intPrimTy, byteArrayPrimTy, -- Integer
     intPrimTy]

integerMonadic name = AlgResult name [] one_Integer_ty integerTyCon []

integerDyadic name = AlgResult name [] two_Integer_tys integerTyCon []

integerDyadic2Results name = AlgResult name [] two_Integer_tys return2GMPsTyCon []

integerCompare name = PrimResult name [] two_Integer_tys intPrimTyCon IntRep []
\end{code}

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.

\begin{code}
primOpInfo :: PrimOp -> PrimOpInfo
\end{code}

There's plenty of this stuff!

%************************************************************************
%*									*
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
%*									*
%************************************************************************

\begin{code}
primOpInfo CharGtOp   = Compare SLIT("gtChar#")   charPrimTy
primOpInfo CharGeOp   = Compare SLIT("geChar#")   charPrimTy
primOpInfo CharEqOp   = Compare SLIT("eqChar#")   charPrimTy
primOpInfo CharNeOp   = Compare SLIT("neChar#")   charPrimTy
primOpInfo CharLtOp   = Compare SLIT("ltChar#")   charPrimTy
primOpInfo CharLeOp   = Compare SLIT("leChar#")   charPrimTy

primOpInfo IntGtOp    = Compare SLIT("gtInt#")	   intPrimTy
primOpInfo IntGeOp    = Compare SLIT("geInt#")	   intPrimTy
primOpInfo IntEqOp    = Compare SLIT("eqInt#")	   intPrimTy
primOpInfo IntNeOp    = Compare SLIT("neInt#")	   intPrimTy
primOpInfo IntLtOp    = Compare SLIT("ltInt#")	   intPrimTy
primOpInfo IntLeOp    = Compare SLIT("leInt#")	   intPrimTy

primOpInfo WordGtOp   = Compare SLIT("gtWord#")   wordPrimTy
primOpInfo WordGeOp   = Compare SLIT("geWord#")   wordPrimTy
primOpInfo WordEqOp   = Compare SLIT("eqWord#")   wordPrimTy
primOpInfo WordNeOp   = Compare SLIT("neWord#")   wordPrimTy
primOpInfo WordLtOp   = Compare SLIT("ltWord#")   wordPrimTy
primOpInfo WordLeOp   = Compare SLIT("leWord#")   wordPrimTy

primOpInfo AddrGtOp   = Compare SLIT("gtAddr#")   addrPrimTy
primOpInfo AddrGeOp   = Compare SLIT("geAddr#")   addrPrimTy
primOpInfo AddrEqOp   = Compare SLIT("eqAddr#")   addrPrimTy
primOpInfo AddrNeOp   = Compare SLIT("neAddr#")   addrPrimTy
primOpInfo AddrLtOp   = Compare SLIT("ltAddr#")   addrPrimTy
primOpInfo AddrLeOp   = Compare SLIT("leAddr#")   addrPrimTy

primOpInfo FloatGtOp  = Compare SLIT("gtFloat#")  floatPrimTy
primOpInfo FloatGeOp  = Compare SLIT("geFloat#")  floatPrimTy
primOpInfo FloatEqOp  = Compare SLIT("eqFloat#")  floatPrimTy
primOpInfo FloatNeOp  = Compare SLIT("neFloat#")  floatPrimTy
primOpInfo FloatLtOp  = Compare SLIT("ltFloat#")  floatPrimTy
primOpInfo FloatLeOp  = Compare SLIT("leFloat#")  floatPrimTy

primOpInfo DoubleGtOp = Compare SLIT("gtDouble#") doublePrimTy
primOpInfo DoubleGeOp = Compare SLIT("geDouble#") doublePrimTy
primOpInfo DoubleEqOp = Compare SLIT("eqDouble#") doublePrimTy
primOpInfo DoubleNeOp = Compare SLIT("neDouble#") doublePrimTy
primOpInfo DoubleLtOp = Compare SLIT("ltDouble#") doublePrimTy
primOpInfo DoubleLeOp = Compare SLIT("leDouble#") doublePrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Char]{PrimOpInfo for @Char#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo OrdOp = Coercing SLIT("ord#") charPrimTy intPrimTy
primOpInfo ChrOp = Coercing SLIT("chr#") intPrimTy charPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Int]{PrimOpInfo for @Int#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo IntAddOp  = Dyadic SLIT("plusInt#")	 intPrimTy
primOpInfo IntSubOp  = Dyadic SLIT("minusInt#") intPrimTy
primOpInfo IntMulOp  = Dyadic SLIT("timesInt#") intPrimTy
primOpInfo IntQuotOp = Dyadic SLIT("quotInt#")	 intPrimTy
primOpInfo IntRemOp  = Dyadic SLIT("remInt#")	 intPrimTy

primOpInfo IntNegOp  = Monadic SLIT("negateInt#") intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Word]{PrimOpInfo for @Word#@s}
%*									*
%************************************************************************

A @Word#@ is an unsigned @Int#@.

\begin{code}
primOpInfo AndOp    = Dyadic  SLIT("and#")	wordPrimTy
primOpInfo OrOp	    = Dyadic  SLIT("or#")	wordPrimTy
primOpInfo NotOp    = Monadic SLIT("not#")	wordPrimTy

primOpInfo SllOp
  = PrimResult SLIT("shiftL#")  [] [wordPrimTy, intPrimTy] wordPrimTyCon WordRep []
primOpInfo SraOp
  = PrimResult SLIT("shiftRA#") [] [wordPrimTy, intPrimTy] wordPrimTyCon WordRep []
primOpInfo SrlOp
  = PrimResult SLIT("shiftRL#") [] [wordPrimTy, intPrimTy] wordPrimTyCon WordRep []

primOpInfo ISllOp
  = PrimResult SLIT("iShiftL#")  [] [intPrimTy, intPrimTy] intPrimTyCon IntRep []
primOpInfo ISraOp
  = PrimResult SLIT("iShiftRA#") [] [intPrimTy, intPrimTy] intPrimTyCon IntRep []
primOpInfo ISrlOp
  = PrimResult SLIT("iShiftRL#") [] [intPrimTy, intPrimTy] intPrimTyCon IntRep []

primOpInfo Int2WordOp = Coercing SLIT("int2Word#") intPrimTy wordPrimTy
primOpInfo Word2IntOp = Coercing SLIT("word2Int#") wordPrimTy intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Addr]{PrimOpInfo for @Addr#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo Int2AddrOp = Coercing SLIT("int2Addr#") intPrimTy addrPrimTy
primOpInfo Addr2IntOp = Coercing SLIT("addr2Int#") addrPrimTy intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Float]{PrimOpInfo for @Float#@s}
%*									*
%************************************************************************

@encodeFloat#@ and @decodeFloat#@ are given w/ Integer-stuff (it's
similar).

\begin{code}
primOpInfo FloatAddOp	= Dyadic    SLIT("plusFloat#")	   floatPrimTy
primOpInfo FloatSubOp	= Dyadic    SLIT("minusFloat#")   floatPrimTy
primOpInfo FloatMulOp	= Dyadic    SLIT("timesFloat#")   floatPrimTy
primOpInfo FloatDivOp	= Dyadic    SLIT("divideFloat#")  floatPrimTy
primOpInfo FloatNegOp	= Monadic   SLIT("negateFloat#")  floatPrimTy

primOpInfo Float2IntOp	= Coercing SLIT("float2Int#") floatPrimTy intPrimTy
primOpInfo Int2FloatOp	= Coercing SLIT("int2Float#") intPrimTy floatPrimTy

primOpInfo FloatExpOp	= Monadic   SLIT("expFloat#")	   floatPrimTy
primOpInfo FloatLogOp	= Monadic   SLIT("logFloat#")	   floatPrimTy
primOpInfo FloatSqrtOp	= Monadic   SLIT("sqrtFloat#")	   floatPrimTy
primOpInfo FloatSinOp	= Monadic   SLIT("sinFloat#")	   floatPrimTy
primOpInfo FloatCosOp	= Monadic   SLIT("cosFloat#")	   floatPrimTy
primOpInfo FloatTanOp	= Monadic   SLIT("tanFloat#")	   floatPrimTy
primOpInfo FloatAsinOp	= Monadic   SLIT("asinFloat#")	   floatPrimTy
primOpInfo FloatAcosOp	= Monadic   SLIT("acosFloat#")	   floatPrimTy
primOpInfo FloatAtanOp	= Monadic   SLIT("atanFloat#")	   floatPrimTy
primOpInfo FloatSinhOp	= Monadic   SLIT("sinhFloat#")	   floatPrimTy
primOpInfo FloatCoshOp	= Monadic   SLIT("coshFloat#")	   floatPrimTy
primOpInfo FloatTanhOp	= Monadic   SLIT("tanhFloat#")	   floatPrimTy
primOpInfo FloatPowerOp	= Dyadic    SLIT("powerFloat#")   floatPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Double]{PrimOpInfo for @Double#@s}
%*									*
%************************************************************************

@encodeDouble#@ and @decodeDouble#@ are given w/ Integer-stuff (it's
similar).

\begin{code}
primOpInfo DoubleAddOp	= Dyadic    SLIT("plusDouble#")   doublePrimTy
primOpInfo DoubleSubOp	= Dyadic    SLIT("minusDouble#")  doublePrimTy
primOpInfo DoubleMulOp	= Dyadic    SLIT("timesDouble#")  doublePrimTy
primOpInfo DoubleDivOp	= Dyadic    SLIT("divideDouble#") doublePrimTy
primOpInfo DoubleNegOp	= Monadic   SLIT("negateDouble#") doublePrimTy

primOpInfo Double2IntOp	    = Coercing SLIT("double2Int#")   doublePrimTy intPrimTy
primOpInfo Int2DoubleOp	    = Coercing SLIT("int2Double#")   intPrimTy doublePrimTy

primOpInfo Double2FloatOp   = Coercing SLIT("double2Float#") doublePrimTy floatPrimTy
primOpInfo Float2DoubleOp   = Coercing SLIT("float2Double#") floatPrimTy doublePrimTy

primOpInfo DoubleExpOp	= Monadic   SLIT("expDouble#")	   doublePrimTy
primOpInfo DoubleLogOp	= Monadic   SLIT("logDouble#")	   doublePrimTy
primOpInfo DoubleSqrtOp	= Monadic   SLIT("sqrtDouble#")   doublePrimTy
primOpInfo DoubleSinOp	= Monadic   SLIT("sinDouble#")	   doublePrimTy
primOpInfo DoubleCosOp	= Monadic   SLIT("cosDouble#")	   doublePrimTy
primOpInfo DoubleTanOp	= Monadic   SLIT("tanDouble#")	   doublePrimTy
primOpInfo DoubleAsinOp	= Monadic   SLIT("asinDouble#")   doublePrimTy
primOpInfo DoubleAcosOp	= Monadic   SLIT("acosDouble#")   doublePrimTy
primOpInfo DoubleAtanOp	= Monadic   SLIT("atanDouble#")   doublePrimTy
primOpInfo DoubleSinhOp	= Monadic   SLIT("sinhDouble#")   doublePrimTy
primOpInfo DoubleCoshOp	= Monadic   SLIT("coshDouble#")   doublePrimTy
primOpInfo DoubleTanhOp	= Monadic   SLIT("tanhDouble#")   doublePrimTy
primOpInfo DoublePowerOp= Dyadic    SLIT("powerDouble#")  doublePrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Integer]{PrimOpInfo for @Integer@ (and related!)}
%*									*
%************************************************************************

\begin{code}
primOpInfo IntegerNegOp	= integerMonadic SLIT("negateInteger#")

primOpInfo IntegerAddOp	= integerDyadic SLIT("plusInteger#")
primOpInfo IntegerSubOp	= integerDyadic SLIT("minusInteger#")
primOpInfo IntegerMulOp	= integerDyadic SLIT("timesInteger#")

primOpInfo IntegerCmpOp	= integerCompare SLIT("cmpInteger#")

primOpInfo IntegerQuotRemOp = integerDyadic2Results SLIT("quotRemInteger#")
primOpInfo IntegerDivModOp  = integerDyadic2Results SLIT("divModInteger#")

primOpInfo Integer2IntOp
  = PrimResult SLIT("integer2Int#") [] one_Integer_ty intPrimTyCon IntRep []

primOpInfo Int2IntegerOp
  = AlgResult SLIT("int2Integer#") [] [intPrimTy] integerTyCon []

primOpInfo Word2IntegerOp
  = AlgResult SLIT("word2Integer#") [] [wordPrimTy] integerTyCon []

primOpInfo Addr2IntegerOp
  = AlgResult SLIT("addr2Integer#") [] [addrPrimTy] integerTyCon []
\end{code}

Encoding and decoding of floating-point numbers is sorta
Integer-related.

\begin{code}
primOpInfo FloatEncodeOp
  = PrimResult SLIT("encodeFloat#") [] an_Integer_and_Int_tys
	 floatPrimTyCon FloatRep []

primOpInfo DoubleEncodeOp
  = PrimResult SLIT("encodeDouble#") [] an_Integer_and_Int_tys
	doublePrimTyCon DoubleRep []

primOpInfo FloatDecodeOp
  = AlgResult SLIT("decodeFloat#") [] [floatPrimTy] returnIntAndGMPTyCon []

primOpInfo DoubleDecodeOp
  = AlgResult SLIT("decodeDouble#") [] [doublePrimTy] returnIntAndGMPTyCon []
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Arrays]{PrimOpInfo for primitive arrays}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("newArray#") [s_tv, elt_tv] [intPrimTy, elt, mkStatePrimTy s]
				stateAndMutableArrayPrimTyCon [s, elt]

primOpInfo (NewByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	(str, _, prim_tycon) = getPrimRepInfo kind

	op_str	       = _PK_ ("new" ++ str ++ "Array#")
    in
    AlgResult op_str [s_tv]
	[intPrimTy, mkStatePrimTy s]
	stateAndMutableByteArrayPrimTyCon [s]

---------------------------------------------------------------------------

primOpInfo SameMutableArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	mut_arr_ty = mkMutableArrayPrimTy s elt
    } in
    AlgResult SLIT("sameMutableArray#") [s_tv, elt_tv] [mut_arr_ty, mut_arr_ty]
				   boolTyCon []

primOpInfo SameMutableByteArrayOp
  = let {
	s = alphaTy; s_tv = alphaTyVar;
	mut_arr_ty = mkMutableByteArrayPrimTy s
    } in
    AlgResult SLIT("sameMutableByteArray#") [s_tv] [mut_arr_ty, mut_arr_ty]
				   boolTyCon []

---------------------------------------------------------------------------
-- Primitive arrays of Haskell pointers:

primOpInfo ReadArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("readArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]


primOpInfo WriteArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    PrimResult SLIT("writeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, elt, mkStatePrimTy s]
	statePrimTyCon VoidRep [s]

primOpInfo IndexArrayOp
  = let { elt = alphaTy; elt_tv = alphaTyVar } in
    AlgResult SLIT("indexArray#") [elt_tv] [mkArrayPrimTy elt, intPrimTy]
				   liftTyCon [elt]

---------------------------------------------------------------------------
-- Primitive arrays full of unboxed bytes:

primOpInfo (ReadByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	(str, _, prim_tycon) = getPrimRepInfo kind

	op_str	       = _PK_ ("read" ++ str ++ "Array#")
	relevant_tycon = assoc "primOpInfo" tbl kind
    in
    AlgResult op_str [s_tv]
	[mkMutableByteArrayPrimTy s, intPrimTy, mkStatePrimTy s]
	relevant_tycon [s]
  where
    tbl = [ (CharRep,	 stateAndCharPrimTyCon),
	    (IntRep,	 stateAndIntPrimTyCon),
	    (AddrRep,	 stateAndAddrPrimTyCon),
	    (FloatRep,	 stateAndFloatPrimTyCon),
	    (DoubleRep, stateAndDoublePrimTyCon) ]

  -- How come there's no Word byte arrays? ADR

primOpInfo (WriteByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	(str, prim_ty, _) = getPrimRepInfo kind
	op_str = _PK_ ("write" ++ str ++ "Array#")
    in
    -- NB: *Prim*Result --
    PrimResult op_str [s_tv]
	[mkMutableByteArrayPrimTy s, intPrimTy, prim_ty, mkStatePrimTy s]
	statePrimTyCon VoidRep [s]

primOpInfo (IndexByteArrayOp kind)
  = let
	(str, _, prim_tycon) = getPrimRepInfo kind
	op_str = _PK_ ("index" ++ str ++ "Array#")
    in
    -- NB: *Prim*Result --
    PrimResult op_str [] [byteArrayPrimTy, intPrimTy] prim_tycon kind []

primOpInfo (IndexOffAddrOp kind)
  = let
	(str, _, prim_tycon) = getPrimRepInfo kind
	op_str = _PK_ ("index" ++ str ++ "OffAddr#")
    in
    PrimResult op_str [] [addrPrimTy, intPrimTy] prim_tycon kind []

---------------------------------------------------------------------------
primOpInfo UnsafeFreezeArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("unsafeFreezeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, mkStatePrimTy s]
	stateAndArrayPrimTyCon [s, elt]

primOpInfo UnsafeFreezeByteArrayOp
  = let { s = alphaTy; s_tv = alphaTyVar } in
    AlgResult SLIT("unsafeFreezeByteArray#") [s_tv]
	[mkMutableByteArrayPrimTy s, mkStatePrimTy s]
	stateAndByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-SynchVars]{PrimOpInfo for synchronizing Variables}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewSynchVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("newSynchVar#") [s_tv, elt_tv] [mkStatePrimTy s]
				stateAndSynchVarPrimTyCon [s, elt]

primOpInfo TakeMVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("takeMVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]

primOpInfo PutMVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("putMVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, elt, mkStatePrimTy s]
	statePrimTyCon [s]

primOpInfo ReadIVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("readIVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]

primOpInfo WriteIVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    AlgResult SLIT("writeIVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, elt, mkStatePrimTy s]
	statePrimTyCon [s]

\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Wait]{PrimOpInfo for delay/wait operations}
%*									*
%************************************************************************

\begin{code}

primOpInfo DelayOp
  = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
    PrimResult SLIT("delay#") [s_tv]
	[intPrimTy, mkStatePrimTy s]
	statePrimTyCon VoidRep [s]

primOpInfo WaitReadOp
  = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
    PrimResult SLIT("waitRead#") [s_tv]
	[intPrimTy, mkStatePrimTy s]
	statePrimTyCon VoidRep [s]

primOpInfo WaitWriteOp
  = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
    PrimResult SLIT("waitWrite#") [s_tv]
	[intPrimTy, mkStatePrimTy s]
	statePrimTyCon VoidRep [s]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-makeForeignObj]{PrimOpInfo for Foreign Objects}
%*									*
%************************************************************************

Not everything should/can be in the Haskell heap. As an example, in an
image processing application written in Haskell, you really would like
to avoid heaving huge images between different space or generations of
a garbage collector. Instead use @ForeignObj@ (formerly known as @MallocPtr@),
which refer to some externally allocated structure/value. Using @ForeignObj@,
just a reference to an image is present in the heap, the image could then
be stored outside the Haskell heap, i.e., as a malloc'ed structure or in
a completely separate address space alltogether. 

When a @ForeignObj@ becomes garbage, a user-defined finalisation routine
associated with the object is invoked (currently, each ForeignObj has a
direct reference to its finaliser).  -- SOF

The only function defined over @ForeignObj@s is:

\begin{pseudocode}
makeForeignObj# :: Addr#  -- foreign object
                -> Addr#  -- ptr to its finaliser routine
		-> StateAndForeignObj# _RealWorld# ForeignObj#
\end{pseudocode}

\begin{code}
primOpInfo MakeForeignObjOp
  = AlgResult SLIT("makeForeignObj#") [] 
	[addrPrimTy, addrPrimTy, realWorldStatePrimTy] 
	stateAndForeignObjPrimTyCon [realWorldTy]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-stable-pointers]{PrimOpInfo for ``stable pointers''}
%*									*
%************************************************************************

A {\em stable pointer} is an index into a table of pointers into the
heap.  Since the garbage collector is told about stable pointers, it
is safe to pass a stable pointer to external systems such as C
routines.

Here's what the operations and types are supposed to be (from
state-interface document).

\begin{verbatim}
makeStablePtr#  :: a -> State# _RealWorld -> StateAndStablePtr# _RealWorld a
freeStablePtr#  :: StablePtr# a -> State# _RealWorld -> State# _RealWorld
deRefStablePtr# :: StablePtr# a -> State# _RealWorld -> StateAndPtr _RealWorld a
\end{verbatim}

It may seem a bit surprising that @makeStablePtr#@ is a @PrimIO@
operation since it doesn't (directly) involve IO operations.  The
reason is that if some optimisation pass decided to duplicate calls to
@makeStablePtr#@ and we only pass one of the stable pointers over, a
massive space leak can result.  Putting it into the PrimIO monad
prevents this.  (Another reason for putting them in a monad is to
ensure correct sequencing wrt the side-effecting @freeStablePtr#@
operation.)

Note that we can implement @freeStablePtr#@ using @_ccall_@ (and,
besides, it's not likely to be used from Haskell) so it's not a
primop.

Question: Why @_RealWorld@ - won't any instance of @_ST@ do the job? [ADR]

\begin{code}
primOpInfo MakeStablePtrOp
  = AlgResult SLIT("makeStablePtr#") [alphaTyVar]
	[alphaTy, realWorldStatePrimTy]
	stateAndStablePtrPrimTyCon [realWorldTy, alphaTy]

primOpInfo DeRefStablePtrOp
  = AlgResult SLIT("deRefStablePtr#") [alphaTyVar]
	[mkStablePtrPrimTy alphaTy, realWorldStatePrimTy]
	stateAndPtrPrimTyCon [realWorldTy, alphaTy]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-unsafePointerEquality]{PrimOpInfo for Pointer Equality}
%*									*
%************************************************************************

[Alastair Reid is to blame for this!]

These days, (Glasgow) Haskell seems to have a bit of everything from
other languages: strict operations, mutable variables, sequencing,
pointers, etc.  About the only thing left is LISP's ability to test
for pointer equality.  So, let's add it in!

\begin{verbatim}
reallyUnsafePtrEquality :: a -> a -> Int#
\end{verbatim}

which tests any two closures (of the same type) to see if they're the
same.  (Returns $0$ for @False@, $\neq 0$ for @True@ - to avoid
difficulties of trying to box up the result.)

NB This is {\em really unsafe\/} because even something as trivial as
a garbage collection might change the answer by removing indirections.
Still, no-one's forcing you to use it.  If you're worried about little
things like loss of referential transparency, you might like to wrap
it all up in a monad-like thing as John O'Donnell and John Hughes did
for non-determinism (1989 (Fraserburgh) Glasgow FP Workshop
Proceedings?)

I'm thinking of using it to speed up a critical equality test in some
graphics stuff in a context where the possibility of saying that
denotationally equal things aren't isn't a problem (as long as it
doesn't happen too often.)  ADR

To Will: Jim said this was already in, but I can't see it so I'm
adding it.  Up to you whether you add it.  (Note that this could have
been readily implemented using a @veryDangerousCCall@ before they were
removed...)

\begin{code}
primOpInfo ReallyUnsafePtrEqualityOp
  = PrimResult SLIT("reallyUnsafePtrEquality#") [alphaTyVar]
	[alphaTy, alphaTy] intPrimTyCon IntRep []
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-parallel]{PrimOpInfo for parallelism op(s)}
%*									*
%************************************************************************

\begin{code}
primOpInfo SeqOp	-- seq# :: a -> Int#
  = PrimResult SLIT("seq#")	[alphaTyVar] [alphaTy] intPrimTyCon IntRep []

primOpInfo ParOp	-- par# :: a -> Int#
  = PrimResult SLIT("par#")	[alphaTyVar] [alphaTy] intPrimTyCon IntRep []

primOpInfo ForkOp	-- fork# :: a -> Int#
  = PrimResult SLIT("fork#")	[alphaTyVar] [alphaTy] intPrimTyCon IntRep []

\end{code}

\begin{code}
-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#

primOpInfo ParGlobalOp	-- parGlobal# :: Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = PrimResult SLIT("parGlobal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTyCon IntRep []   -- liftTyCon [betaTy]

primOpInfo ParLocalOp	-- parLocal# :: Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = PrimResult SLIT("parLocal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTyCon IntRep []   -- liftTyCon [betaTy]

primOpInfo ParAtOp	-- parAt# :: Int# -> Int# -> Int# -> Int# -> a -> b -> c -> c
  = PrimResult SLIT("parAt#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTyCon IntRep []   -- liftTyCon [gammaTy]

primOpInfo ParAtAbsOp	-- parAtAbs# :: Int# -> Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = PrimResult SLIT("parAtAbs#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTyCon IntRep []   -- liftTyCon [betaTy]

primOpInfo ParAtRelOp	-- parAtRel# :: Int# -> Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = PrimResult SLIT("parAtRel#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTyCon IntRep []   -- liftTyCon [betaTy]

primOpInfo ParAtForNowOp	-- parAtForNow# :: Int# -> Int# -> Int# -> Int# -> a -> b -> c -> c
  = PrimResult SLIT("parAtForNow#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTyCon IntRep []   -- liftTyCon [gammaTy]

primOpInfo CopyableOp	-- copyable# :: a -> a
  = PrimResult SLIT("copyable#")	[alphaTyVar] [alphaTy] intPrimTyCon IntRep []   -- liftTyCon [alphaTy]

primOpInfo NoFollowOp	-- noFollow# :: a -> a
  = PrimResult SLIT("noFollow#")	[alphaTyVar] [alphaTy] intPrimTyCon IntRep []   -- liftTyCon [alphaTy]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-errorIO]{PrimOpInfo for @errorIO#@}
%*									*
%************************************************************************

\begin{code}
primOpInfo ErrorIOPrimOp -- errorIO# :: PrimIO () -> State# RealWorld#
  = PrimResult SLIT("errorIO#") []
	[primio_ish_ty unitTy]
	statePrimTyCon VoidRep [realWorldTy]
  where
    primio_ish_ty result
      = mkFunTy (mkStateTy realWorldTy) (mkTupleTy 2 [result, mkStateTy realWorldTy])
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-IO-etc]{PrimOpInfo for C calls, and I/O-ish things}
%*									*
%************************************************************************

\begin{code}
primOpInfo (CCallOp _ _ _ arg_tys result_ty)
  = AlgResult SLIT("ccall#") [] arg_tys result_tycon tys_applied
  where
    (result_tycon, tys_applied, _) = --trace "PrimOp.getAppDataTyConExpandingDicts" $
				     getAppDataTyConExpandingDicts result_ty

#ifdef DEBUG
primOpInfo op = panic ("primOpInfo:"++ show (I# (tagOf_PrimOp op)))
#endif
\end{code}

%************************************************************************
%*									*
\subsection[PrimOp-utils]{Utilities for @PrimitiveOps@}
%*									*
%************************************************************************

The primitive-array-creation @PrimOps@ and {\em most} of those to do
with @Integers@ can trigger GC.  Here we describe the heap requirements
of the various @PrimOps@.  For most, no heap is required.  For a few,
a fixed amount of heap is required, and the needs of the @PrimOp@ can
be combined with the rest of the heap usage in the basic block.  For an
unfortunate few, some unknown amount of heap is required (these are the
ops which can trigger GC).

\begin{code}
data HeapRequirement
    = NoHeapRequired
    | FixedHeapRequired HeapOffset
    | VariableHeapRequired

primOpHeapReq :: PrimOp -> HeapRequirement

primOpHeapReq NewArrayOp	= VariableHeapRequired
primOpHeapReq (NewByteArrayOp _)= VariableHeapRequired

primOpHeapReq IntegerAddOp	= VariableHeapRequired
primOpHeapReq IntegerSubOp	= VariableHeapRequired
primOpHeapReq IntegerMulOp	= VariableHeapRequired
primOpHeapReq IntegerQuotRemOp	= VariableHeapRequired
primOpHeapReq IntegerDivModOp	= VariableHeapRequired
primOpHeapReq IntegerNegOp	= VariableHeapRequired
primOpHeapReq Int2IntegerOp	= FixedHeapRequired
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE))
primOpHeapReq Word2IntegerOp	= FixedHeapRequired
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE))
primOpHeapReq Addr2IntegerOp	= VariableHeapRequired
primOpHeapReq FloatDecodeOp	= FixedHeapRequired
				  (addOff (intOff (getPrimRepSize IntRep + mP_STRUCT_SIZE))
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE)))
primOpHeapReq DoubleDecodeOp	= FixedHeapRequired
				  (addOff (intOff (getPrimRepSize IntRep + mP_STRUCT_SIZE))
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE)))

-- ccall may allocate heap if it is explicitly allowed to (_ccall_gc_)
-- or if it returns a ForeignObj.

primOpHeapReq (CCallOp _ _ mayGC@True  _ _) = VariableHeapRequired
primOpHeapReq (CCallOp _ _ mayGC@False _ _) = NoHeapRequired

primOpHeapReq MakeForeignObjOp	= VariableHeapRequired

-- this occasionally has to expand the Stable Pointer table
primOpHeapReq MakeStablePtrOp	= VariableHeapRequired

-- These four only need heap space with the native code generator
-- ToDo!: parameterize, so we know if native code generation is taking place(JSM)

primOpHeapReq IntegerCmpOp	= FixedHeapRequired (intOff (2 * mP_STRUCT_SIZE))
primOpHeapReq Integer2IntOp    	= FixedHeapRequired (intOff mP_STRUCT_SIZE)
primOpHeapReq FloatEncodeOp    	= FixedHeapRequired (intOff mP_STRUCT_SIZE)
primOpHeapReq DoubleEncodeOp   	= FixedHeapRequired (intOff mP_STRUCT_SIZE)

-- a NewSynchVarOp creates a three-word mutuple in the heap.
primOpHeapReq NewSynchVarOp	= FixedHeapRequired
    	    	    	    	  (addOff (totHdrSize (MuTupleRep 3)) (intOff 3))

-- Sparking ops no longer allocate any heap; however, _fork_ may
-- require a context switch to clear space in the required thread
-- pool, and that requires liveness information.

primOpHeapReq ParOp	    	= NoHeapRequired
primOpHeapReq ForkOp	    	= VariableHeapRequired

-- A SeqOp requires unknown space to evaluate its argument
primOpHeapReq SeqOp	    	= VariableHeapRequired

-- GranSim sparks are stgMalloced i.e. no heap required
primOpHeapReq ParGlobalOp	= NoHeapRequired
primOpHeapReq ParLocalOp	= NoHeapRequired
primOpHeapReq ParAtOp	        = NoHeapRequired
primOpHeapReq ParAtAbsOp	= NoHeapRequired
primOpHeapReq ParAtRelOp	= NoHeapRequired
primOpHeapReq ParAtForNowOp	= NoHeapRequired
-- CopyableOp and NoFolowOp don't require heap; don't rely on default
primOpHeapReq CopyableOp	= NoHeapRequired
primOpHeapReq NoFollowOp	= NoHeapRequired

primOpHeapReq other_op	    	= NoHeapRequired
\end{code}

The amount of stack required by primops.

\begin{code}
data StackRequirement
  = NoStackRequired 
  | FixedStackRequired Int {-AStack-} Int {-BStack-}
  | VariableStackRequired
     
primOpStackRequired SeqOp = FixedStackRequired 0 {-AStack-} 2 {-BStack-}
primOpStackRequired _     = VariableStackRequired 
-- ToDo: be more specific for certain primops (currently only used for seq)
\end{code}

Primops which can trigger GC have to be called carefully.
In particular, their arguments are guaranteed to be in registers,
and a liveness mask tells which regs are live.

\begin{code}
primOpCanTriggerGC op
  = case op of
    	TakeMVarOp  -> True
    	ReadIVarOp  -> True
	DelayOp     -> True
	WaitReadOp  -> True
	WaitWriteOp -> True
	_           ->
	    case primOpHeapReq op of
    	    	VariableHeapRequired -> True
    	    	_                    -> False
\end{code}

Sometimes we may choose to execute a PrimOp even though it isn't
certain that its result will be required; ie execute them
``speculatively''.  The same thing as ``cheap eagerness.'' Usually
this is OK, because PrimOps are usually cheap, but it isn't OK for
(a)~expensive PrimOps and (b)~PrimOps which can fail.

See also @primOpIsCheap@ (below).

There should be no worries about side effects; that's all taken care
of by data dependencies.

\begin{code}
primOpOkForSpeculation :: PrimOp -> Bool

-- Int.
primOpOkForSpeculation IntQuotOp	= False		-- Divide by zero
primOpOkForSpeculation IntRemOp		= False		-- Divide by zero

-- Integer
primOpOkForSpeculation IntegerQuotRemOp = False		-- Divide by zero
primOpOkForSpeculation IntegerDivModOp	= False		-- Divide by zero

-- Float.  ToDo: tan? tanh?
primOpOkForSpeculation FloatDivOp	= False		-- Divide by zero
primOpOkForSpeculation FloatLogOp	= False		-- Log of zero
primOpOkForSpeculation FloatAsinOp	= False		-- Arg out of domain
primOpOkForSpeculation FloatAcosOp	= False		-- Arg out of domain

-- Double.  ToDo: tan? tanh?
primOpOkForSpeculation DoubleDivOp	= False		-- Divide by zero
primOpOkForSpeculation DoubleLogOp	= False		-- Log of zero
primOpOkForSpeculation DoubleAsinOp	= False		-- Arg out of domain
primOpOkForSpeculation DoubleAcosOp	= False		-- Arg out of domain

-- CCall
primOpOkForSpeculation (CCallOp	_ _ _ _ _)= False	-- Could be expensive!

-- errorIO#
primOpOkForSpeculation ErrorIOPrimOp	= False		-- Could be disastrous!

-- parallel
primOpOkForSpeculation ParOp		= False	    	-- Could be expensive!
primOpOkForSpeculation ForkOp	    	= False	    	-- Likewise
primOpOkForSpeculation SeqOp	    	= False	    	-- Likewise

primOpOkForSpeculation ParGlobalOp	= False	    	-- Could be expensive!
primOpOkForSpeculation ParLocalOp	= False	    	-- Could be expensive!
primOpOkForSpeculation ParAtOp		= False	    	-- Could be expensive!
primOpOkForSpeculation ParAtAbsOp	= False	    	-- Could be expensive!
primOpOkForSpeculation ParAtRelOp	= False	    	-- Could be expensive!
primOpOkForSpeculation ParAtForNowOp	= False	    	-- Could be expensive!
primOpOkForSpeculation CopyableOp	= False	    	-- only tags closure
primOpOkForSpeculation NoFollowOp	= False	    	-- only tags closure

-- The default is "yes it's ok for speculation"
primOpOkForSpeculation other_op		= True
\end{code}

@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.
\begin{code}
primOpIsCheap op
  = primOpOkForSpeculation op && not (primOpCanTriggerGC op)
\end{code}

And some primops have side-effects and so, for example, must not be
duplicated.

\begin{code}
fragilePrimOp :: PrimOp -> Bool

fragilePrimOp ParOp = True
fragilePrimOp ForkOp = True
fragilePrimOp SeqOp = True
fragilePrimOp MakeForeignObjOp = True  -- SOF
fragilePrimOp MakeStablePtrOp  = True
fragilePrimOp DeRefStablePtrOp = True  -- ??? JSM & ADR

fragilePrimOp ParGlobalOp = True
fragilePrimOp ParLocalOp = True
fragilePrimOp ParAtOp = True
fragilePrimOp ParAtAbsOp = True
fragilePrimOp ParAtRelOp = True
fragilePrimOp ParAtForNowOp = True
fragilePrimOp CopyableOp = True  -- Possibly not.  ASP 
fragilePrimOp NoFollowOp = True  -- Possibly not.  ASP

fragilePrimOp other = False
\end{code}

Primitive operations that perform calls need wrappers to save any live variables
that are stored in caller-saves registers

\begin{code}
primOpNeedsWrapper :: PrimOp -> Bool

primOpNeedsWrapper (CCallOp _ _ _ _ _) 	= True

primOpNeedsWrapper NewArrayOp     	= True	-- ToDo: for nativeGen only!(JSM)
primOpNeedsWrapper (NewByteArrayOp _)  	= True

primOpNeedsWrapper IntegerAddOp		= True
primOpNeedsWrapper IntegerSubOp		= True
primOpNeedsWrapper IntegerMulOp		= True
primOpNeedsWrapper IntegerQuotRemOp	= True
primOpNeedsWrapper IntegerDivModOp	= True
primOpNeedsWrapper IntegerNegOp		= True
primOpNeedsWrapper IntegerCmpOp	    	= True
primOpNeedsWrapper Integer2IntOp    	= True
primOpNeedsWrapper Int2IntegerOp	= True
primOpNeedsWrapper Word2IntegerOp	= True
primOpNeedsWrapper Addr2IntegerOp	= True

primOpNeedsWrapper FloatExpOp	    	= True
primOpNeedsWrapper FloatLogOp	    	= True
primOpNeedsWrapper FloatSqrtOp	    	= True
primOpNeedsWrapper FloatSinOp	    	= True
primOpNeedsWrapper FloatCosOp	    	= True
primOpNeedsWrapper FloatTanOp	    	= True
primOpNeedsWrapper FloatAsinOp	    	= True
primOpNeedsWrapper FloatAcosOp	    	= True
primOpNeedsWrapper FloatAtanOp	    	= True
primOpNeedsWrapper FloatSinhOp	    	= True
primOpNeedsWrapper FloatCoshOp	    	= True
primOpNeedsWrapper FloatTanhOp	    	= True
primOpNeedsWrapper FloatPowerOp	    	= True
primOpNeedsWrapper FloatEncodeOp    	= True
primOpNeedsWrapper FloatDecodeOp	= True

primOpNeedsWrapper DoubleExpOp	    	= True
primOpNeedsWrapper DoubleLogOp	    	= True
primOpNeedsWrapper DoubleSqrtOp	    	= True
primOpNeedsWrapper DoubleSinOp	    	= True
primOpNeedsWrapper DoubleCosOp	    	= True
primOpNeedsWrapper DoubleTanOp	    	= True
primOpNeedsWrapper DoubleAsinOp	    	= True
primOpNeedsWrapper DoubleAcosOp	    	= True
primOpNeedsWrapper DoubleAtanOp	    	= True
primOpNeedsWrapper DoubleSinhOp	    	= True
primOpNeedsWrapper DoubleCoshOp	    	= True
primOpNeedsWrapper DoubleTanhOp	    	= True
primOpNeedsWrapper DoublePowerOp    	= True
primOpNeedsWrapper DoubleEncodeOp   	= True
primOpNeedsWrapper DoubleDecodeOp	= True

primOpNeedsWrapper MakeForeignObjOp	= True
primOpNeedsWrapper MakeStablePtrOp	= True
primOpNeedsWrapper DeRefStablePtrOp	= True

primOpNeedsWrapper TakeMVarOp	    	= True
primOpNeedsWrapper PutMVarOp		= True
primOpNeedsWrapper ReadIVarOp	    	= True

primOpNeedsWrapper DelayOp	    	= True
primOpNeedsWrapper WaitReadOp		= True
primOpNeedsWrapper WaitWriteOp		= True

primOpNeedsWrapper other_op 	    	= False
\end{code}

\begin{code}
primOp_str op
  = case (primOpInfo op) of
      Dyadic     str _	       -> str
      Monadic    str _	       -> str
      Compare    str _	       -> str
      Coercing   str _ _       -> str
      PrimResult str _ _ _ _ _ -> str
      AlgResult  str _ _ _ _   -> str
\end{code}

@primOpType@ duplicates some work of @primOpId@, but since we
grab types pretty often...
\begin{code}
primOpType :: PrimOp -> Type

primOpType op
  = case (primOpInfo op) of
      Dyadic str ty ->	    dyadic_fun_ty ty
      Monadic str ty ->	    monadic_fun_ty ty
      Compare str ty ->	    compare_fun_ty ty
      Coercing str ty1 ty2 -> mkFunTy ty1 ty2

      PrimResult str tyvars arg_tys prim_tycon kind res_tys ->
	mkForAllTys tyvars (mkFunTys arg_tys (applyTyCon prim_tycon res_tys))

      AlgResult str tyvars arg_tys tycon res_tys ->
	mkForAllTys tyvars (mkFunTys arg_tys (applyTyCon tycon res_tys))
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim	    PrimRep
  | ReturnsAlg	    TyCon

-- ToDo: Deal with specialised PrimOps
--	 Will need to return specialised tycon and data constructors

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo

getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty		 -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty		 -> ReturnsPrim (typePrimRep ty)
      Compare _ ty		 -> ReturnsAlg  boolTyCon
      Coercing  _ _ ty		 -> ReturnsPrim (typePrimRep ty)
      PrimResult _ _ _ _ kind _	 -> ReturnsPrim kind
      AlgResult _ _ _ tycon _	 -> ReturnsAlg  tycon

isCompareOp :: PrimOp -> Bool

isCompareOp op
  = case primOpInfo op of
      Compare _ _ -> True
      _	    	  -> False
\end{code}

The commutable ops are those for which we will try to move constants
to the right hand side for strength reduction.

\begin{code}
commutableOp :: PrimOp -> Bool

commutableOp CharEqOp	  = True
commutableOp CharNeOp 	  = True
commutableOp IntAddOp 	  = True
commutableOp IntMulOp 	  = True
commutableOp AndOp	  = True
commutableOp OrOp	  = True
commutableOp IntEqOp	  = True
commutableOp IntNeOp	  = True
commutableOp IntegerAddOp = True
commutableOp IntegerMulOp = True
commutableOp FloatAddOp	  = True
commutableOp FloatMulOp	  = True
commutableOp FloatEqOp	  = True
commutableOp FloatNeOp	  = True
commutableOp DoubleAddOp  = True
commutableOp DoubleMulOp  = True
commutableOp DoubleEqOp	  = True
commutableOp DoubleNeOp	  = True
commutableOp _		  = False
\end{code}

Utils:
\begin{code}
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] boolTy
\end{code}

Output stuff:
\begin{code}
pprPrimOp  :: PprStyle -> PrimOp -> Pretty
showPrimOp :: PprStyle -> PrimOp -> String

showPrimOp sty op
  = ppShow 1000{-random-} (pprPrimOp sty op)

pprPrimOp sty (CCallOp fun is_casm may_gc arg_tys res_ty)
  = let
	before
	  = if is_casm then
	       if may_gc then "(_casm_GC_ ``" else "(_casm_ ``"
	    else
	       if may_gc then "(_ccall_GC_ " else "(_ccall_ "

	after
	  = if is_casm then ppStr "''" else ppNil

	pp_tys
	  = ppBesides [ppStr " { [",
		ppIntersperse pp'SP{-'-} (map (pprParendGenType sty) arg_tys),
		ppRbrack, ppSP, pprParendGenType sty res_ty, ppStr " })"]

    in
    ppBesides [ppStr before, ppPStr fun, after, pp_tys]

pprPrimOp sty other_op
  = let
	str = primOp_str other_op
    in
    (if codeStyle sty then identToC else ppPStr) str

instance Outputable PrimOp where
    ppr sty op = pprPrimOp sty op
\end{code}
