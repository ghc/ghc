%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[PrimOps]{Primitive operations (machine-level)}

\begin{code}
#include "HsVersions.h"

module PrimOps (
	PrimOp(..),
	tagOf_PrimOp, -- ToDo: rm
	primOpNameInfo, primOpId,
	typeOfPrimOp, isCompareOp,
	primOpCanTriggerGC, primOpNeedsWrapper,
    	primOpOkForSpeculation, primOpIsCheap,
	fragilePrimOp,

	PrimOpResultInfo(..),
	getPrimOpResultInfo,

    	HeapRequirement(..), primOpHeapReq, 

       -- export for the Native Code Generator
--      primOpInfo, not exported
        PrimOpInfo(..),

	pprPrimOp, showPrimOp,

	-- and to make the interface self-sufficient....
	PrimKind, HeapOffset, Id, Name, TyCon, UniType, TyVarTemplate
    ) where

import PrelFuns		-- help stuff for prelude
import PrimKind		-- most of it
import TysPrim
import TysWiredIn

import AbsUniType	-- lots of things
import CLabelInfo	( identToC )
import CgCompInfo   	( mIN_MP_INT_SIZE, mP_STRUCT_SIZE )
import BasicLit		( BasicLit(..) )
import HeapOffs	    	( addOff, intOff, totHdrSize, HeapOffset )
import Id		-- lots
import IdInfo		-- plenty of this, too
import Maybes		( Maybe(..) )
import NameTypes	( mkPreludeCoreName, FullName, ShortName )
import Outputable
import PlainCore	-- all of it
import Pretty
import SMRep	    	( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import Unique
import Util
#ifdef DPH
import TyPod
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PrimOps-datatype]{Datatype for @PrimOp@ (an enumeration)}
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
    | IntDivOp{-UNUSED-} | IntRemOp | IntNegOp | IntAbsOp

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
    | NewByteArrayOp PrimKind

    | SameMutableArrayOp
    | SameMutableByteArrayOp

    | ReadArrayOp | WriteArrayOp | IndexArrayOp -- for arrays of Haskell ptrs

    | ReadByteArrayOp	PrimKind
    | WriteByteArrayOp	PrimKind
    | IndexByteArrayOp	PrimKind
    | IndexOffAddrOp	PrimKind
	-- PrimKind can be one of {Char,Int,Addr,Float,Double}Kind.
	-- This is just a cheesy encoding of a bunch of ops.
	-- Note that MallocPtrKind is not included -- the only way of
	-- creating a MallocPtr is with a ccall or casm.

    | UnsafeFreezeArrayOp | UnsafeFreezeByteArrayOp

    | NewSynchVarOp -- for MVars and IVars
    | TakeMVarOp | PutMVarOp
    | ReadIVarOp | WriteIVarOp

    | MakeStablePtrOp | DeRefStablePtrOp
\end{code}

A special ``trap-door'' to use in making calls direct to C functions:
\begin{code}
    | CCallOp	FAST_STRING	-- An "unboxed" ccall# to this named function
		Bool		-- True <=> really a "casm"
		Bool		-- True <=> might invoke Haskell GC
		[UniType]	-- Unboxed argument; the state-token
				-- argument will have been put *first*
		UniType		-- Return type; one of the "StateAnd<blah>#" types

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
CoCase 
  ( CoPrim
      (CCallOp "foo" [Universe#, StablePtr# a, Int#] FloatPrimAndUniverse False) 
       -- :: Universe# -> StablePtr# a -> Int# -> FloatPrimAndUniverse
      []
      [w#, sp# i#]
  )
  (CoAlgAlts [ ( FloatPrimAndIoWorld, 
                 [f#, w#], 
                 CoCon (TupleCon 2) [Float, IoWorld] [F# f#, World w#]
               ) ]
             CoNoDefault
  )
\end{pseudocode}

Nota Bene: there are some people who find the empty list of types in
the @CoPrim@ somewhat puzzling and would represent the above by
\begin{pseudocode}
CoCase 
  ( CoPrim
      (CCallOp "foo" [alpha1, alpha2, alpha3] alpha4 False)
       -- :: /\ alpha1, alpha2 alpha3, alpha4. 
       --       alpha1 -> alpha2 -> alpha3 -> alpha4
      [Universe#, StablePtr# a, Int#, FloatPrimAndIoWorld]
      [w#, sp# i#]
  )
  (CoAlgAlts [ ( FloatPrimAndIoWorld, 
                 [f#, w#], 
                 CoCon (TupleCon 2) [Float, IoWorld] [F# f#, World w#]
               ) ]
             CoNoDefault
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

    -- two for concurrency
    | DelayOp
    | WaitOp

#ifdef GRAN
    | ParGlobalOp	-- named global par
    | ParLocalOp	-- named local par
    | ParAtOp		-- specifies destination of local par
    | ParAtForNowOp	-- specifies initial destination of global par
    | CopyableOp	-- marks copyable code
    | NoFollowOp	-- marks non-followup expression
#endif {-GRAN-}

#ifdef DPH
-- Shadow all the the above primitive OPs for N dimensioned objects.
    | PodNPrimOp Int PrimOp

-- Primitive conversion functions.

    | Int2PodNOp Int	 | Char2PodNOp Int    | Float2PodNOp Int
    | Double2PodNOp Int  | String2PodNOp Int

#endif {-Data Parallel Haskell -}
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
--UNUSED:tagOf_PrimOp IntDivOp			= ILIT( 43)
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
tagOf_PrimOp (NewByteArrayOp CharKind)	= ILIT(118)
tagOf_PrimOp (NewByteArrayOp IntKind)	= ILIT(119)
tagOf_PrimOp (NewByteArrayOp AddrKind)	= ILIT(120)
tagOf_PrimOp (NewByteArrayOp FloatKind)	= ILIT(121)
tagOf_PrimOp (NewByteArrayOp DoubleKind)= ILIT(122)
tagOf_PrimOp SameMutableArrayOp		= ILIT(123)
tagOf_PrimOp SameMutableByteArrayOp	= ILIT(124)
tagOf_PrimOp ReadArrayOp		= ILIT(125)
tagOf_PrimOp WriteArrayOp		= ILIT(126)
tagOf_PrimOp IndexArrayOp		= ILIT(127)
tagOf_PrimOp (ReadByteArrayOp CharKind)	    = ILIT(128)
tagOf_PrimOp (ReadByteArrayOp IntKind)	    = ILIT(129)
tagOf_PrimOp (ReadByteArrayOp AddrKind)	    = ILIT(130)
tagOf_PrimOp (ReadByteArrayOp FloatKind)    = ILIT(131)
tagOf_PrimOp (ReadByteArrayOp DoubleKind)   = ILIT(132)
tagOf_PrimOp (WriteByteArrayOp CharKind)    = ILIT(133)
tagOf_PrimOp (WriteByteArrayOp IntKind)	    = ILIT(134)
tagOf_PrimOp (WriteByteArrayOp AddrKind)    = ILIT(135)
tagOf_PrimOp (WriteByteArrayOp FloatKind)   = ILIT(136)
tagOf_PrimOp (WriteByteArrayOp DoubleKind)  = ILIT(137)
tagOf_PrimOp (IndexByteArrayOp CharKind)    = ILIT(138)
tagOf_PrimOp (IndexByteArrayOp IntKind)	    = ILIT(139)
tagOf_PrimOp (IndexByteArrayOp AddrKind)    = ILIT(140)
tagOf_PrimOp (IndexByteArrayOp FloatKind)   = ILIT(141)
tagOf_PrimOp (IndexByteArrayOp DoubleKind)  = ILIT(142)
tagOf_PrimOp (IndexOffAddrOp CharKind)	    = ILIT(143)
tagOf_PrimOp (IndexOffAddrOp IntKind)	    = ILIT(144)
tagOf_PrimOp (IndexOffAddrOp AddrKind)	    = ILIT(145)
tagOf_PrimOp (IndexOffAddrOp FloatKind)	    = ILIT(146)
tagOf_PrimOp (IndexOffAddrOp DoubleKind)    = ILIT(147)
tagOf_PrimOp UnsafeFreezeArrayOp	    = ILIT(148)
tagOf_PrimOp UnsafeFreezeByteArrayOp	    = ILIT(149)
tagOf_PrimOp NewSynchVarOp		    = ILIT(150)
tagOf_PrimOp TakeMVarOp		    	    = ILIT(151)
tagOf_PrimOp PutMVarOp		    	    = ILIT(152)
tagOf_PrimOp ReadIVarOp		    	    = ILIT(153)
tagOf_PrimOp WriteIVarOp		    = ILIT(154)
tagOf_PrimOp MakeStablePtrOp		    = ILIT(155)
tagOf_PrimOp DeRefStablePtrOp		    = ILIT(156)
tagOf_PrimOp (CCallOp _ _ _ _ _)	    = ILIT(157)
tagOf_PrimOp ErrorIOPrimOp		    = ILIT(158)
tagOf_PrimOp ReallyUnsafePtrEqualityOp	    = ILIT(159)
tagOf_PrimOp SeqOp			    = ILIT(160)
tagOf_PrimOp ParOp			    = ILIT(161)
tagOf_PrimOp ForkOp			    = ILIT(162)
tagOf_PrimOp DelayOp			    = ILIT(163)
tagOf_PrimOp WaitOp			    = ILIT(164)

#ifdef GRAN
tagOf_PrimOp ParGlobalOp		    = ILIT(165)
tagOf_PrimOp ParLocalOp			    = ILIT(166)
tagOf_PrimOp ParAtOp			    = ILIT(167)
tagOf_PrimOp ParAtForNowOp		    = ILIT(168)
tagOf_PrimOp CopyableOp			    = ILIT(169)
tagOf_PrimOp NoFollowOp			    = ILIT(170)
#endif {-GRAN-}

#ifdef DPH
tagOf_PrimOp (PodNPrimOp _ _)		= panic "ToDo:DPH:tagOf_PrimOp"
tagOf_PrimOp (Int2PodNOp _)		= panic "ToDo:DPH:tagOf_PrimOp"
tagOf_PrimOp (Char2PodNOp _)		= panic "ToDo:DPH:tagOf_PrimOp"
tagOf_PrimOp (Float2PodNOp  _)		= panic "ToDo:DPH:tagOf_PrimOp"
tagOf_PrimOp (Double2PodNOp _)		= panic "ToDo:DPH:tagOf_PrimOp"
tagOf_PrimOp (String2PodNOp _)		= panic "ToDo:DPH:tagOf_PrimOp"
#endif {-Data Parallel Haskell -}

-- avoid BUG
tagOf_PrimOp _ = case (panic "tagOf_PrimOp: pattern-match") of { o ->
		 tagOf_PrimOp o
		 }

instance Eq PrimOp where
    op == op2 = tagOf_PrimOp op _EQ_ tagOf_PrimOp op2
\end{code}

%************************************************************************
%*									*
\subsection[PrimOps-info]{The essential info about each @PrimOp@}
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
		UniType
  | Monadic	FAST_STRING	-- string :: T -> T
		UniType
  | Compare	FAST_STRING	-- string :: T -> T -> Bool
		UniType
  | Coerce	FAST_STRING	-- string :: T1 -> T2
		UniType
		UniType

  | PrimResult	FAST_STRING
		[TyVarTemplate] [UniType] TyCon PrimKind [UniType]
		-- "PrimResult tvs [t1,..,tn] D# kind [s1,..,sm]"
		-- has type Forall tvs. t1 -> ... -> tn -> (D# s1 ... sm)
		-- D# is a primitive type constructor.
		-- (the kind is the same info as D#, in another convenient form)

  | AlgResult	FAST_STRING
		[TyVarTemplate] [UniType] TyCon [UniType]
		-- "AlgResult tvs [t1,..,tn] T [s1,..,sm]"
		-- has type Forall tvs. t1 -> ... -> tn -> (T s1 ... sm)

-- ToDo: Specialised calls to PrimOps are prohibited but may be desirable

#ifdef DPH
  | PodNInfo	Int
		PrimOpInfo
#endif {- Data Parallel Haskell -}
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

integerCompare name = PrimResult name [] two_Integer_tys intPrimTyCon IntKind []
\end{code}

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.

\begin{code}
primOpInfo :: PrimOp -> PrimOpInfo
\end{code}

There's plenty of this stuff!

%************************************************************************
%*									*
\subsubsection[PrimOps-comparison]{PrimOpInfo basic comparison ops}
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
\subsubsection[PrimOps-Char]{PrimOpInfo for @Char#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo OrdOp = Coerce SLIT("ord#") charPrimTy intPrimTy
primOpInfo ChrOp = Coerce SLIT("chr#") intPrimTy charPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Int]{PrimOpInfo for @Int#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo IntAddOp  = Dyadic SLIT("plusInt#")	 intPrimTy
primOpInfo IntSubOp  = Dyadic SLIT("minusInt#") intPrimTy
primOpInfo IntMulOp  = Dyadic SLIT("timesInt#") intPrimTy
primOpInfo IntQuotOp = Dyadic SLIT("quotInt#")	 intPrimTy
--UNUSED:primOpInfo IntDivOp  = Dyadic SLIT("divInt#")	 intPrimTy
primOpInfo IntRemOp  = Dyadic SLIT("remInt#")	 intPrimTy

primOpInfo IntNegOp  = Monadic SLIT("negateInt#") intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Word]{PrimOpInfo for @Word#@s}
%*									*
%************************************************************************

A @Word#@ is an unsigned @Int#@.

\begin{code}
primOpInfo AndOp    = Dyadic  SLIT("and#")	wordPrimTy
primOpInfo OrOp	    = Dyadic  SLIT("or#")	wordPrimTy
primOpInfo NotOp    = Monadic SLIT("not#")	wordPrimTy

primOpInfo SllOp
  = PrimResult SLIT("shiftL#")  [] [wordPrimTy, intPrimTy] wordPrimTyCon WordKind []
primOpInfo SraOp
  = PrimResult SLIT("shiftRA#") [] [wordPrimTy, intPrimTy] wordPrimTyCon WordKind []
primOpInfo SrlOp
  = PrimResult SLIT("shiftRL#") [] [wordPrimTy, intPrimTy] wordPrimTyCon WordKind []

primOpInfo ISllOp
  = PrimResult SLIT("iShiftL#")  [] [intPrimTy, intPrimTy] intPrimTyCon IntKind []
primOpInfo ISraOp
  = PrimResult SLIT("iShiftRA#") [] [intPrimTy, intPrimTy] intPrimTyCon IntKind []
primOpInfo ISrlOp
  = PrimResult SLIT("iShiftRL#") [] [intPrimTy, intPrimTy] intPrimTyCon IntKind []

primOpInfo Int2WordOp = Coerce SLIT("int2Word#") intPrimTy wordPrimTy
primOpInfo Word2IntOp = Coerce SLIT("word2Int#") wordPrimTy intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Addr]{PrimOpInfo for @Addr#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo Int2AddrOp = Coerce SLIT("int2Addr#") intPrimTy addrPrimTy
primOpInfo Addr2IntOp = Coerce SLIT("addr2Int#") addrPrimTy intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Float]{PrimOpInfo for @Float#@s}
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

primOpInfo Float2IntOp	= Coerce SLIT("float2Int#") floatPrimTy intPrimTy
primOpInfo Int2FloatOp	= Coerce SLIT("int2Float#") intPrimTy floatPrimTy

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
\subsubsection[PrimOps-Double]{PrimOpInfo for @Double#@s}
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

primOpInfo Double2IntOp	    = Coerce SLIT("double2Int#")   doublePrimTy intPrimTy
primOpInfo Int2DoubleOp	    = Coerce SLIT("int2Double#")   intPrimTy doublePrimTy

primOpInfo Double2FloatOp   = Coerce SLIT("double2Float#") doublePrimTy floatPrimTy
primOpInfo Float2DoubleOp   = Coerce SLIT("float2Double#") floatPrimTy doublePrimTy

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
\subsubsection[PrimOps-Integer]{PrimOpInfo for @Integer@ (and related!)}
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
  = PrimResult SLIT("integer2Int#") [] one_Integer_ty intPrimTyCon IntKind []

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
	 floatPrimTyCon FloatKind []

primOpInfo DoubleEncodeOp
  = PrimResult SLIT("encodeDouble#") [] an_Integer_and_Int_tys
	doublePrimTyCon DoubleKind []

primOpInfo FloatDecodeOp
  = AlgResult SLIT("decodeFloat#") [] [floatPrimTy] returnIntAndGMPTyCon []

primOpInfo DoubleDecodeOp
  = AlgResult SLIT("decodeDouble#") [] [doublePrimTy] returnIntAndGMPTyCon []
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Arrays]{PrimOpInfo for primitive arrays}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewArrayOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("newArray#") [s_tv, elt_tv] [intPrimTy, elt, mkStatePrimTy s]
				stateAndMutableArrayPrimTyCon [s, elt]

primOpInfo (NewByteArrayOp kind)
  = let
	s = alpha; s_tv = alpha_tv

	(str, _, prim_tycon) = getKindInfo kind

	op_str	       = _PK_ ("new" ++ str ++ "Array#")
    in
    AlgResult op_str [s_tv] 
        [intPrimTy, mkStatePrimTy s]
	stateAndMutableByteArrayPrimTyCon [s]

---------------------------------------------------------------------------

primOpInfo SameMutableArrayOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv;
	mut_arr_ty = mkMutableArrayPrimTy s elt
    } in
    AlgResult SLIT("sameMutableArray#") [s_tv, elt_tv] [mut_arr_ty, mut_arr_ty]
				   boolTyCon []

primOpInfo SameMutableByteArrayOp
  = let {
	s = alpha; s_tv = alpha_tv;
	mut_arr_ty = mkMutableByteArrayPrimTy s
    } in
    AlgResult SLIT("sameMutableByteArray#") [s_tv] [mut_arr_ty, mut_arr_ty]
				   boolTyCon []

---------------------------------------------------------------------------
-- Primitive arrays of Haskell pointers:

primOpInfo ReadArrayOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("readArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]


primOpInfo WriteArrayOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    PrimResult SLIT("writeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, elt, mkStatePrimTy s]
	statePrimTyCon VoidKind [s]

primOpInfo IndexArrayOp
  = let { elt = alpha; elt_tv = alpha_tv } in
    AlgResult SLIT("indexArray#") [elt_tv] [mkArrayPrimTy elt, intPrimTy]
				   liftTyCon [elt]

---------------------------------------------------------------------------
-- Primitive arrays full of unboxed bytes:

primOpInfo (ReadByteArrayOp kind)
  = let
	s = alpha; s_tv = alpha_tv

	(str, _, prim_tycon) = getKindInfo kind

	op_str	       = _PK_ ("read" ++ str ++ "Array#")
	relevant_tycon = assoc "primOpInfo" tbl kind
    in
    AlgResult op_str [s_tv]
	[mkMutableByteArrayPrimTy s, intPrimTy, mkStatePrimTy s]
	relevant_tycon [s]
  where
    tbl = [ (CharKind,	 stateAndCharPrimTyCon),
	    (IntKind,	 stateAndIntPrimTyCon),
	    (AddrKind,	 stateAndAddrPrimTyCon),
	    (FloatKind,	 stateAndFloatPrimTyCon),
	    (DoubleKind, stateAndDoublePrimTyCon) ]

  -- How come there's no Word byte arrays? ADR

primOpInfo (WriteByteArrayOp kind)
  = let
	s = alpha; s_tv = alpha_tv

	(str, prim_ty, _) = getKindInfo kind
	op_str = _PK_ ("write" ++ str ++ "Array#")
    in
    -- NB: *Prim*Result --
    PrimResult op_str [s_tv]
	[mkMutableByteArrayPrimTy s, intPrimTy, prim_ty, mkStatePrimTy s]
	statePrimTyCon VoidKind [s]

primOpInfo (IndexByteArrayOp kind)
  = let
	(str, _, prim_tycon) = getKindInfo kind
	op_str = _PK_ ("index" ++ str ++ "Array#")
    in
    -- NB: *Prim*Result --
    PrimResult op_str [] [byteArrayPrimTy, intPrimTy] prim_tycon kind []

primOpInfo (IndexOffAddrOp kind)
  = let
	(str, _, prim_tycon) = getKindInfo kind
	op_str = _PK_ ("index" ++ str ++ "OffAddr#")
    in
    PrimResult op_str [] [addrPrimTy, intPrimTy] prim_tycon kind []

---------------------------------------------------------------------------
primOpInfo UnsafeFreezeArrayOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("unsafeFreezeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, mkStatePrimTy s]
	stateAndArrayPrimTyCon [s, elt]

primOpInfo UnsafeFreezeByteArrayOp
  = let { s = alpha; s_tv = alpha_tv } in
    AlgResult SLIT("unsafeFreezeByteArray#") [s_tv]
	[mkMutableByteArrayPrimTy s, mkStatePrimTy s]
	stateAndByteArrayPrimTyCon [s]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-SynchVars]{PrimOpInfo for synchronizing Variables}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewSynchVarOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("newSynchVar#") [s_tv, elt_tv] [mkStatePrimTy s]
				stateAndSynchVarPrimTyCon [s, elt]

primOpInfo TakeMVarOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("takeMVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]

primOpInfo PutMVarOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("putMVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, elt, mkStatePrimTy s]
	statePrimTyCon [s]

primOpInfo ReadIVarOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("readIVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, mkStatePrimTy s]
	stateAndPtrPrimTyCon [s, elt]

primOpInfo WriteIVarOp
  = let {
	elt = alpha; elt_tv = alpha_tv; s = beta; s_tv = beta_tv
    } in
    AlgResult SLIT("writeIVar#") [s_tv, elt_tv]
	[mkSynchVarPrimTy s elt, elt, mkStatePrimTy s]
	statePrimTyCon [s]

\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-Wait]{PrimOpInfo for delay/wait operations}
%*									*
%************************************************************************

\begin{code}

primOpInfo DelayOp
  = let {
	s = alpha; s_tv = alpha_tv
    } in
    PrimResult SLIT("delay#") [s_tv]
	[intPrimTy, mkStatePrimTy s]
	statePrimTyCon VoidKind [s]

primOpInfo WaitOp
  = let {
	s = alpha; s_tv = alpha_tv
    } in
    PrimResult SLIT("wait#") [s_tv]
	[intPrimTy, mkStatePrimTy s]
	statePrimTyCon VoidKind [s]

\end{code}


%************************************************************************
%*									*
\subsubsection[PrimOps-stable-pointers]{PrimOpInfo for ``stable pointers''}
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
  = AlgResult SLIT("makeStablePtr#") [alpha_tv] 
	[alpha, realWorldStatePrimTy] 
	stateAndStablePtrPrimTyCon [realWorldTy, alpha]

primOpInfo DeRefStablePtrOp
  = AlgResult SLIT("deRefStablePtr#") [alpha_tv] 
	[mkStablePtrPrimTy alpha, realWorldStatePrimTy]
	stateAndPtrPrimTyCon [realWorldTy, alpha]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-unsafePointerEquality]{PrimOpInfo for Pointer Equality}
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
  = PrimResult SLIT("reallyUnsafePtrEquality#") [alpha_tv] 
	[alpha, alpha] intPrimTyCon IntKind []
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-parallel]{PrimOpInfo for parallelism op(s)}
%*									*
%************************************************************************

\begin{code}
primOpInfo SeqOp	-- seq# :: a -> Int#
  = PrimResult SLIT("seq#")	[alpha_tv] [alpha] intPrimTyCon IntKind []

primOpInfo ParOp	-- par# :: a -> Int#
  = PrimResult SLIT("par#")	[alpha_tv] [alpha] intPrimTyCon IntKind []

primOpInfo ForkOp	-- fork# :: a -> Int#
  = PrimResult SLIT("fork#")	[alpha_tv] [alpha] intPrimTyCon IntKind []

\end{code}

\begin{code}
#ifdef GRAN

primOpInfo ParGlobalOp	-- parGlobal# :: Int -> a -> b -> b
  = AlgResult SLIT("parGlobal#")	[alpha_tv,beta_tv] [intPrimTy,alpha,beta] liftTyCon [beta]

primOpInfo ParLocalOp	-- parLocal# :: Int -> a -> b -> b
  = AlgResult SLIT("parLocal#")	[alpha_tv,beta_tv] [intPrimTy,alpha,beta] liftTyCon [beta]

primOpInfo ParAtOp	-- parAt# :: Int -> a -> b -> c -> c
  = AlgResult SLIT("parAt#")	[alpha_tv,beta_tv,gamma_tv] [intPrimTy,alpha,beta,gamma] liftTyCon [gamma]

primOpInfo ParAtForNowOp	-- parAtForNow# :: Int -> a -> b -> c -> c
  = AlgResult SLIT("parAtForNow#")	[alpha_tv,beta_tv,gamma_tv] [intPrimTy,alpha,beta,gamma] liftTyCon [gamma]

primOpInfo CopyableOp	-- copyable# :: a -> a
  = AlgResult SLIT("copyable#")	[alpha_tv] [alpha] liftTyCon [alpha]

primOpInfo NoFollowOp	-- noFollow# :: a -> a
  = AlgResult SLIT("noFollow#")	[alpha_tv] [alpha] liftTyCon [alpha]

#endif {-GRAN-}
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-errorIO]{PrimOpInfo for @errorIO#@}
%*									*
%************************************************************************

\begin{code}
primOpInfo ErrorIOPrimOp -- errorIO# :: PrimIO () -> State# RealWorld#
  = PrimResult SLIT("errorIO#") []
	[mkPrimIoTy unitTy]
	statePrimTyCon VoidKind [realWorldTy]
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-IO-etc]{PrimOpInfo for C calls, and I/O-ish things}
%*									*
%************************************************************************

\begin{code}
primOpInfo (CCallOp _ _ _ arg_tys result_ty)
  = AlgResult SLIT("ccall#") [] arg_tys result_tycon tys_applied
  where
    (result_tycon, tys_applied, _) = getUniDataTyCon result_ty
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOps-DPH]{PrimOpInfo for Data Parallel Haskell}
%*									*
%************************************************************************

\begin{code}
#ifdef DPH
-- ToDo:DPH: various things need doing here

primOpInfo (Int2PodNOp d) =	Coerce ("int2Pod" ++ show d)
				       IntKind
				       (PodNKind d IntKind)

primOpInfo (Char2PodNOp d) =	Coerce ("char2Pod" ++ show d)
				       CharKind
				       (PodNKind d CharKind)

primOpInfo (Float2PodNOp d) =	Coerce ("float2Pod" ++ show d)
				       FloatKind
				       (PodNKind d FloatKind)

primOpInfo (Double2PodNOp d) =	Coerce ("double2Pod" ++ show d)
				       DoubleKind
				       (PodNKind d DoubleKind)

{-
primOpInfo (Integer2PodNOp d) = Coerce ("integer2Pod" ++ show d)
				       IntegerKind
				       (PodNKind d IntegerKind)
-}

primOpInfo (String2PodNOp d) =	Coerce ("string2Pod" ++ show d)
				       LitStringKind
				       (PodNKind d LitStringKind)

primOpInfo (PodNPrimOp d p) = PodNInfo d (primOpInfo p)
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PrimOps-utils]{Utilities for @PrimitiveOps@}
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
                                  (addOff (intOff (getKindSize IntKind + mP_STRUCT_SIZE))
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE)))
primOpHeapReq DoubleDecodeOp	= FixedHeapRequired 
                                  (addOff (intOff (getKindSize IntKind + mP_STRUCT_SIZE))
    	    	    	    	  (addOff (totHdrSize (DataRep mIN_MP_INT_SIZE))
    	    	    	    	    	  (intOff mIN_MP_INT_SIZE)))

-- ccall may allocate heap if it is explicitly allowed to (_ccall_gc_)
-- or if it returns a MallocPtr.

primOpHeapReq (CCallOp _ _ mayGC@True _ _) = VariableHeapRequired
primOpHeapReq (CCallOp _ _ mayGC@False _ return_ty)
   = if returnsMallocPtr
     then VariableHeapRequired
     else NoHeapRequired
  where
   returnsMallocPtr
     = case (getUniDataTyCon_maybe return_ty) of
	 Nothing            -> False
	 Just (tycon, _, _) -> tycon == stateAndMallocPtrPrimTyCon

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

#ifdef GRAN

-- a ParGlobalOp creates a single 4-tuple in the heap.  ToDo: verify this!
primOpHeapReq ParGlobalOp	= trace "primOpHeapReq:ParGlobalOp:verify!" (
				  FixedHeapRequired
    	    	    	    	  (addOff (totHdrSize (MuTupleRep 4)) (intOff 4))
				  )

-- a ParLocalOp creates a single 4-tuple in the heap.  ToDo: verify this!
primOpHeapReq ParLocalOp	= trace "primOpHeapReq:ParLocalOp:verify!" (
				  FixedHeapRequired
    	    	    	    	  (addOff (totHdrSize (MuTupleRep 4)) (intOff 4))
				  )

-- ToDo: parAt, parAtForNow, copyable, noFollow !!  (HWL)
#endif {-GRAN-}

primOpHeapReq other_op	    	= NoHeapRequired
\end{code}

Primops which can trigger GC have to be called carefully.
In particular, their arguments are guaranteed to be in registers, 
and a liveness mask tells which regs are live.

\begin{code}
primOpCanTriggerGC op = 
    case op of
    	TakeMVarOp  -> True
    	ReadIVarOp  -> True
	DelayOp     -> True
	WaitOp      -> True
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
--UNUSED:primOpOkForSpeculation IntDivOp		= False		-- Divide by zero
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

#ifdef GRAN
primOpOkForSpeculation ParGlobalOp	= False	    	-- Could be expensive!
primOpOkForSpeculation ParLocalOp	= False	    	-- Could be expensive!
#endif {-GRAN-}

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
fragilePrimOp MakeStablePtrOp = True
fragilePrimOp DeRefStablePtrOp = True  -- ??? JSM & ADR

#ifdef GRAN
fragilePrimOp ParGlobalOp = True
fragilePrimOp ParLocalOp = True
fragilePrimOp CopyableOp = trace "fragilePrimOp:CopyableOp" True  -- Possibly not.  ASP
fragilePrimOp NoFollowOp = trace "fragilePrimOp:NoFollowOp" True  -- Possibly not.  ASP
#endif {-GRAN-}

fragilePrimOp other = False
\end{code}

Primitive operations that perform calls need wrappers to save any live variables
that are stored in caller-saves registers

\begin{code}
primOpNeedsWrapper :: PrimOp -> Bool

primOpNeedsWrapper (CCallOp _ _ _ _ _) 	= True

--UNUSED:primOpNeedsWrapper IntDivOp 	    	= True

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

primOpNeedsWrapper MakeStablePtrOp	= True
primOpNeedsWrapper DeRefStablePtrOp	= True

primOpNeedsWrapper TakeMVarOp	    	= True
primOpNeedsWrapper PutMVarOp		= True
primOpNeedsWrapper ReadIVarOp	    	= True

primOpNeedsWrapper DelayOp	    	= True
primOpNeedsWrapper WaitOp		= True

primOpNeedsWrapper other_op 	    	= False
\end{code}

\begin{code}
primOpId	:: PrimOp -> Id
primOpNameInfo	:: PrimOp -> (FAST_STRING, Name)

-- the *NameInfo ones are trivial:

primOpNameInfo	op = (primOp_str  op, WiredInVal (primOpId op))

primOp_str op
  = case (primOpInfo op) of
      Dyadic str _	       -> str
      Monadic str _	       -> str
      Compare str _	       -> str
      Coerce str _ _	       -> str 
      PrimResult str _ _ _ _ _ -> str
      AlgResult str _ _ _ _    -> str
#ifdef DPH
      PodNInfo d i -> case i of
			Dyadic str _	-> (str ++ ".POD" ++ show d ++ "#")
			Monadic str _	-> (str ++ ".POD" ++ show d ++ "#")
			Compare str _	-> (str ++ ".POD" ++ show d ++ "#")
			Coerce str _ _	-> (str ++ ".POD" ++ show d ++ "#")
			PrimResult str _ _ _ _ _ -> (str ++ ".POD" ++ show d)
			AlgResult str _ _ _ _	-> (str ++ ".POD" ++ show d)
#endif {- Data Parallel Haskell -}
\end{code}

@typeOfPrimOp@ duplicates some work of @primOpId@, but since we
grab types pretty often...
\begin{code}
typeOfPrimOp :: PrimOp -> UniType

#ifdef DPH
typeOfPrimOp (PodNPrimOp d p)
  = mkPodizedPodNTy d (typeOfPrimOp p)
#endif {- Data Parallel Haskell -}

typeOfPrimOp op
  = case (primOpInfo op) of
      Dyadic str ty ->	    dyadic_fun_ty ty
      Monadic str ty ->	    monadic_fun_ty ty
      Compare str ty ->	    prim_compare_fun_ty ty
      Coerce str ty1 ty2 -> UniFun ty1 ty2

      PrimResult str tyvars arg_tys prim_tycon kind res_tys ->
	mkForallTy tyvars (glueTyArgs arg_tys (applyTyCon prim_tycon res_tys))

      AlgResult str tyvars arg_tys tycon res_tys ->
	mkForallTy tyvars (glueTyArgs arg_tys (applyTyCon tycon res_tys))
\end{code}

\begin{code}
primOpId op
  = case (primOpInfo op) of
      Dyadic str ty ->
	mk_prim_Id op pRELUDE_BUILTIN str [] [ty,ty] (dyadic_fun_ty ty) 2

      Monadic str ty ->
	mk_prim_Id op pRELUDE_BUILTIN str [] [ty] (monadic_fun_ty ty) 1

      Compare str ty ->
	mk_prim_Id op pRELUDE_BUILTIN str [] [ty,ty] (prim_compare_fun_ty ty) 2

      Coerce str ty1 ty2 ->
	mk_prim_Id op pRELUDE_BUILTIN str [] [ty1] (UniFun ty1 ty2) 1

      PrimResult str tyvars arg_tys prim_tycon kind res_tys ->
	mk_prim_Id op pRELUDE_BUILTIN str
	    tyvars
	    arg_tys
	    (mkForallTy tyvars (glueTyArgs arg_tys (applyTyCon prim_tycon res_tys)))
	    (length arg_tys) -- arity

      AlgResult str tyvars arg_tys tycon res_tys ->
	mk_prim_Id op pRELUDE_BUILTIN str
	    tyvars
	    arg_tys
	    (mkForallTy tyvars (glueTyArgs arg_tys (applyTyCon tycon res_tys)))
	    (length arg_tys) -- arity

#ifdef DPH
      PodNInfo d i -> panic "primOpId : Oi lazy, PodNInfo needs sorting out"
#endif {- Data Parallel Haskell -}
  where
    mk_prim_Id prim_op mod name tyvar_tmpls arg_tys ty arity
      = mkPreludeId
	    (mkPrimOpIdUnique prim_op)
	    (mkPreludeCoreName mod name)
	    ty
	    (noIdInfo
		`addInfo` (mkArityInfo arity)
		`addInfo_UF` (mkUnfolding EssentialUnfolding
				(mk_prim_unfold prim_op tyvar_tmpls arg_tys)))
\end{code}

The functions to make common unfoldings are tedious.

\begin{code}
mk_prim_unfold :: PrimOp -> [TyVarTemplate] -> [UniType] -> PlainCoreExpr{-template-}

mk_prim_unfold prim_op tv_tmpls arg_tys
  = let
	(inst_env, tyvars, tyvar_tys) = instantiateTyVarTemplates tv_tmpls (map getTheUnique tv_tmpls)
	inst_arg_tys		      = map (instantiateTauTy inst_env) arg_tys
	vars	    		      = mkTemplateLocals inst_arg_tys
    in
    foldr CoTyLam (mkCoLam vars
			   (CoPrim prim_op tyvar_tys [CoVarAtom v | v <- vars]))
		  tyvars
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim	    PrimKind
  | ReturnsAlg	    TyCon

-- ToDo: Deal with specialised PrimOps
--	 Will need to return specialised tycon and data constructors

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo

getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty		 -> ReturnsPrim (kindFromType ty)
      Monadic _ ty		 -> ReturnsPrim (kindFromType ty)
      Compare _ ty		 -> ReturnsAlg  boolTyCon
      Coerce  _ _ ty		 -> ReturnsPrim (kindFromType ty)
      PrimResult _ _ _ _ kind _	 -> ReturnsPrim kind
      AlgResult _ _ _ tycon _	 -> ReturnsAlg  tycon
#ifdef DPH
      PodNInfo d i		 -> panic "getPrimOpResultInfo:PodNInfo"
#endif {- Data Parallel Haskell -}

isCompareOp :: PrimOp -> Bool

isCompareOp op
  = case primOpInfo op of
      Compare _ _ -> True
      _	    	  -> False
\end{code}

Utils:
\begin{code}
dyadic_fun_ty ty    = ty `UniFun` (ty `UniFun` ty)
monadic_fun_ty ty   = ty `UniFun` ty

compare_fun_ty ty	= ty `UniFun` (ty `UniFun` boolTy)
prim_compare_fun_ty ty	= ty `UniFun` (ty `UniFun` boolTy)
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
		ppIntersperse pp'SP{-'-} (map (pprParendUniType sty) arg_tys),
		ppRbrack, ppSP, pprParendUniType sty res_ty, ppStr " })"]

    in
    ppBesides [ppStr before, ppPStr fun, after, pp_tys]
#ifdef DPH
  = fun	-- Comment buggers up machine code :-) -- ToDo:DPH
#endif {- Data Parallel Haskell -}

pprPrimOp sty other_op
  = let
	str = primOp_str other_op
    in
    if codeStyle sty
    then identToC str
    else ppPStr str

instance Outputable PrimOp where
    ppr sty op = pprPrimOp sty op
\end{code}
