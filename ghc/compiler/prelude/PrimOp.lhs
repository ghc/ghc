%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrimOp]{Primitive operations (machine-level)}

\begin{code}
module PrimOp (
	PrimOp(..), allThePrimOps,
	tagOf_PrimOp, -- ToDo: rm
	primOpType,
	primOpUniq, primOpOcc,

	commutableOp,

	primOpOutOfLine, primOpNeedsWrapper, primOpStrictness,
	primOpOkForSpeculation, primOpIsCheap,
	primOpHasSideEffects,

	getPrimOpResultInfo,  PrimOpResultInfo(..),

	pprPrimOp
    ) where

#include "HsVersions.h"

import PrimRep		-- most of it
import TysPrim
import TysWiredIn

import Demand		( Demand, wwLazy, wwPrim, wwStrict )
import Var		( TyVar )
import CallConv		( CallConv, pprCallConv )
import PprType		( pprParendType )
import OccName		( OccName, pprOccName, varOcc )
import TyCon		( TyCon )
import Type		( mkForAllTys, mkForAllTy, mkFunTy, mkFunTys, 
			  mkTyConApp, typePrimRep,
			  splitAlgTyConApp, Type, isUnboxedTupleType, 
			  splitAlgTyConApp_maybe
			)
import Unique		( Unique, mkPrimOpIdUnique )
import Outputable
import Util		( assoc )
import GlaExts		( Int(..), Int#, (==#) )
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
    | ISllOp | ISraOp | ISrlOp -- shift {left,right} {arithmetic,logical}

    -- Word#-related ops:
    | WordQuotOp | WordRemOp
    | AndOp  | OrOp   | NotOp | XorOp
    | SllOp  | SrlOp  -- shift {left,right} {logical}
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
    | IntegerAddOp | IntegerSubOp | IntegerMulOp | IntegerGcdOp
    | IntegerQuotRemOp | IntegerDivModOp | IntegerNegOp

    | IntegerCmpOp

    | Integer2IntOp  | Integer2WordOp  
    | Int2IntegerOp  | Word2IntegerOp
    | Addr2IntegerOp
     -- casting to/from Integer and 64-bit (un)signed quantities.
    | IntegerToInt64Op | Int64ToIntegerOp
    | IntegerToWord64Op | Word64ToIntegerOp
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
    | WriteOffAddrOp    PrimRep
	-- PrimRep can be one of {Char,Int,Addr,Float,Double}Kind.
	-- This is just a cheesy encoding of a bunch of ops.
	-- Note that ForeignObjRep is not included -- the only way of
	-- creating a ForeignObj is with a ccall or casm.
    | IndexOffForeignObjOp PrimRep

    | UnsafeFreezeArrayOp | UnsafeFreezeByteArrayOp
    | SizeofByteArrayOp   | SizeofMutableByteArrayOp

    -- Mutable variables
    | NewMutVarOp
    | ReadMutVarOp
    | WriteMutVarOp
    | SameMutVarOp

    -- for MVars
    | NewMVarOp
    | TakeMVarOp 
    | PutMVarOp
    | SameMVarOp

    -- exceptions
    | CatchOp
    | RaiseOp

    | MakeForeignObjOp
    | WriteForeignObjOp

    | MkWeakOp
    | DeRefWeakOp

    | MakeStablePtrOp
    | DeRefStablePtrOp
    | EqStablePtrOp
\end{code}

A special ``trap-door'' to use in making calls direct to C functions:
\begin{code}
    | CCallOp	(Either 
		    FAST_STRING    -- Left fn => An "unboxed" ccall# to `fn'.
		    Unique)        -- Right u => first argument (an Addr#) is the function pointer
				   --   (unique is used to generate a 'typedef' to cast
				   --    the function pointer if compiling the ccall# down to
				   --    .hc code - can't do this inline for tedious reasons.)
				    
		Bool		    -- True <=> really a "casm"
		Bool		    -- True <=> might invoke Haskell GC
		CallConv	    -- calling convention to use.

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

    -- Operation to test two closure addresses for equality (yes really!)
    -- BLAME ALASTAIR REID FOR THIS!  THE REST OF US ARE INNOCENT!
    | ReallyUnsafePtrEqualityOp

    -- parallel stuff
    | SeqOp
    | ParOp

    -- concurrency
    | ForkOp
    | KillThreadOp
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

Used for the Ord instance

\begin{code}
tagOf_PrimOp CharGtOp			      = (ILIT( 1) :: FAST_INT)
tagOf_PrimOp CharGeOp			      = ILIT(  2)
tagOf_PrimOp CharEqOp			      = ILIT(  3)
tagOf_PrimOp CharNeOp			      = ILIT(  4)
tagOf_PrimOp CharLtOp			      = ILIT(  5)
tagOf_PrimOp CharLeOp			      = ILIT(  6)
tagOf_PrimOp IntGtOp			      = ILIT(  7)
tagOf_PrimOp IntGeOp			      = ILIT(  8)
tagOf_PrimOp IntEqOp			      = ILIT(  9)
tagOf_PrimOp IntNeOp			      = ILIT( 10)
tagOf_PrimOp IntLtOp			      = ILIT( 11)
tagOf_PrimOp IntLeOp			      = ILIT( 12)
tagOf_PrimOp WordGtOp			      = ILIT( 13)
tagOf_PrimOp WordGeOp			      = ILIT( 14)
tagOf_PrimOp WordEqOp			      = ILIT( 15)
tagOf_PrimOp WordNeOp			      = ILIT( 16)
tagOf_PrimOp WordLtOp			      = ILIT( 17)
tagOf_PrimOp WordLeOp			      = ILIT( 18)
tagOf_PrimOp AddrGtOp			      = ILIT( 19)
tagOf_PrimOp AddrGeOp			      = ILIT( 20)
tagOf_PrimOp AddrEqOp			      = ILIT( 21)
tagOf_PrimOp AddrNeOp			      = ILIT( 22)
tagOf_PrimOp AddrLtOp			      = ILIT( 23)
tagOf_PrimOp AddrLeOp			      = ILIT( 24)
tagOf_PrimOp FloatGtOp			      = ILIT( 25)
tagOf_PrimOp FloatGeOp			      = ILIT( 26)
tagOf_PrimOp FloatEqOp			      = ILIT( 27)
tagOf_PrimOp FloatNeOp			      = ILIT( 28)
tagOf_PrimOp FloatLtOp			      = ILIT( 29)
tagOf_PrimOp FloatLeOp			      = ILIT( 30)
tagOf_PrimOp DoubleGtOp			      = ILIT( 31)
tagOf_PrimOp DoubleGeOp			      = ILIT( 32)
tagOf_PrimOp DoubleEqOp			      = ILIT( 33)
tagOf_PrimOp DoubleNeOp			      = ILIT( 34)
tagOf_PrimOp DoubleLtOp			      = ILIT( 35)
tagOf_PrimOp DoubleLeOp			      = ILIT( 36)
tagOf_PrimOp OrdOp			      = ILIT( 37)
tagOf_PrimOp ChrOp			      = ILIT( 38)
tagOf_PrimOp IntAddOp			      = ILIT( 39)
tagOf_PrimOp IntSubOp			      = ILIT( 40)
tagOf_PrimOp IntMulOp			      = ILIT( 41)
tagOf_PrimOp IntQuotOp			      = ILIT( 42)
tagOf_PrimOp IntRemOp			      = ILIT( 43)
tagOf_PrimOp IntNegOp			      = ILIT( 44)
tagOf_PrimOp IntAbsOp			      = ILIT( 45)
tagOf_PrimOp WordQuotOp			      = ILIT( 46)
tagOf_PrimOp WordRemOp			      = ILIT( 47)
tagOf_PrimOp AndOp			      = ILIT( 48)
tagOf_PrimOp OrOp			      = ILIT( 49)
tagOf_PrimOp NotOp			      = ILIT( 50)
tagOf_PrimOp XorOp			      = ILIT( 51)
tagOf_PrimOp SllOp			      = ILIT( 52)
tagOf_PrimOp SrlOp			      = ILIT( 53)
tagOf_PrimOp ISllOp			      = ILIT( 54)
tagOf_PrimOp ISraOp			      = ILIT( 55)
tagOf_PrimOp ISrlOp			      = ILIT( 56)
tagOf_PrimOp Int2WordOp			      = ILIT( 57)
tagOf_PrimOp Word2IntOp			      = ILIT( 58)
tagOf_PrimOp Int2AddrOp			      = ILIT( 59)
tagOf_PrimOp Addr2IntOp			      = ILIT( 60)

tagOf_PrimOp FloatAddOp			      = ILIT( 61)
tagOf_PrimOp FloatSubOp			      = ILIT( 62)
tagOf_PrimOp FloatMulOp			      = ILIT( 63)
tagOf_PrimOp FloatDivOp			      = ILIT( 64)
tagOf_PrimOp FloatNegOp			      = ILIT( 65)
tagOf_PrimOp Float2IntOp		      = ILIT( 66)
tagOf_PrimOp Int2FloatOp		      = ILIT( 67)
tagOf_PrimOp FloatExpOp			      = ILIT( 68)
tagOf_PrimOp FloatLogOp			      = ILIT( 69)
tagOf_PrimOp FloatSqrtOp		      = ILIT( 70)
tagOf_PrimOp FloatSinOp			      = ILIT( 71)
tagOf_PrimOp FloatCosOp			      = ILIT( 72)
tagOf_PrimOp FloatTanOp			      = ILIT( 73)
tagOf_PrimOp FloatAsinOp		      = ILIT( 74)
tagOf_PrimOp FloatAcosOp		      = ILIT( 75)
tagOf_PrimOp FloatAtanOp		      = ILIT( 76)
tagOf_PrimOp FloatSinhOp		      = ILIT( 77)
tagOf_PrimOp FloatCoshOp		      = ILIT( 78)
tagOf_PrimOp FloatTanhOp		      = ILIT( 79)
tagOf_PrimOp FloatPowerOp		      = ILIT( 80)

tagOf_PrimOp DoubleAddOp		      = ILIT( 81)
tagOf_PrimOp DoubleSubOp		      = ILIT( 82)
tagOf_PrimOp DoubleMulOp		      = ILIT( 83)
tagOf_PrimOp DoubleDivOp		      = ILIT( 84)
tagOf_PrimOp DoubleNegOp		      = ILIT( 85)
tagOf_PrimOp Double2IntOp		      = ILIT( 86)
tagOf_PrimOp Int2DoubleOp		      = ILIT( 87)
tagOf_PrimOp Double2FloatOp		      = ILIT( 88)
tagOf_PrimOp Float2DoubleOp		      = ILIT( 89)
tagOf_PrimOp DoubleExpOp		      = ILIT( 90)
tagOf_PrimOp DoubleLogOp		      = ILIT( 91)
tagOf_PrimOp DoubleSqrtOp		      = ILIT( 92)
tagOf_PrimOp DoubleSinOp		      = ILIT( 93)
tagOf_PrimOp DoubleCosOp		      = ILIT( 94)
tagOf_PrimOp DoubleTanOp		      = ILIT( 95)
tagOf_PrimOp DoubleAsinOp		      = ILIT( 96)
tagOf_PrimOp DoubleAcosOp		      = ILIT( 97)
tagOf_PrimOp DoubleAtanOp		      = ILIT( 98)
tagOf_PrimOp DoubleSinhOp		      = ILIT( 99)
tagOf_PrimOp DoubleCoshOp		      = ILIT(100)
tagOf_PrimOp DoubleTanhOp		      = ILIT(101)
tagOf_PrimOp DoublePowerOp		      = ILIT(102)

tagOf_PrimOp IntegerAddOp		      = ILIT(103)
tagOf_PrimOp IntegerSubOp		      = ILIT(104)
tagOf_PrimOp IntegerMulOp		      = ILIT(105)
tagOf_PrimOp IntegerGcdOp		      = ILIT(106)
tagOf_PrimOp IntegerQuotRemOp		      = ILIT(107)
tagOf_PrimOp IntegerDivModOp		      = ILIT(108)
tagOf_PrimOp IntegerNegOp		      = ILIT(109)
tagOf_PrimOp IntegerCmpOp		      = ILIT(110)
tagOf_PrimOp Integer2IntOp		      = ILIT(111)
tagOf_PrimOp Integer2WordOp		      = ILIT(112)
tagOf_PrimOp Int2IntegerOp		      = ILIT(113)
tagOf_PrimOp Word2IntegerOp		      = ILIT(114)
tagOf_PrimOp Addr2IntegerOp		      = ILIT(115)
tagOf_PrimOp IntegerToInt64Op		      = ILIT(116)
tagOf_PrimOp Int64ToIntegerOp		      = ILIT(117)
tagOf_PrimOp IntegerToWord64Op		      = ILIT(118)
tagOf_PrimOp Word64ToIntegerOp		      = ILIT(119)

tagOf_PrimOp FloatEncodeOp		      = ILIT(120)
tagOf_PrimOp FloatDecodeOp		      = ILIT(121)
tagOf_PrimOp DoubleEncodeOp		      = ILIT(122)
tagOf_PrimOp DoubleDecodeOp		      = ILIT(123)

tagOf_PrimOp NewArrayOp			      = ILIT(124)
tagOf_PrimOp (NewByteArrayOp CharRep)	      = ILIT(125)
tagOf_PrimOp (NewByteArrayOp IntRep)	      = ILIT(126)
tagOf_PrimOp (NewByteArrayOp WordRep)	      = ILIT(127)
tagOf_PrimOp (NewByteArrayOp AddrRep)	      = ILIT(128)
tagOf_PrimOp (NewByteArrayOp FloatRep)	      = ILIT(129)
tagOf_PrimOp (NewByteArrayOp DoubleRep)       = ILIT(130)
tagOf_PrimOp (NewByteArrayOp StablePtrRep)    = ILIT(131)
tagOf_PrimOp SameMutableArrayOp		      = ILIT(132)
tagOf_PrimOp SameMutableByteArrayOp	      = ILIT(133)
tagOf_PrimOp ReadArrayOp		      = ILIT(134)
tagOf_PrimOp WriteArrayOp		      = ILIT(135)
tagOf_PrimOp IndexArrayOp		      = ILIT(136)

tagOf_PrimOp (ReadByteArrayOp CharRep)	      = ILIT(137)
tagOf_PrimOp (ReadByteArrayOp IntRep)	      = ILIT(138)
tagOf_PrimOp (ReadByteArrayOp WordRep)	      = ILIT(139)
tagOf_PrimOp (ReadByteArrayOp AddrRep)	      = ILIT(140)
tagOf_PrimOp (ReadByteArrayOp FloatRep)       = ILIT(141)
tagOf_PrimOp (ReadByteArrayOp DoubleRep)      = ILIT(142)
tagOf_PrimOp (ReadByteArrayOp StablePtrRep)   = ILIT(143)
tagOf_PrimOp (ReadByteArrayOp Int64Rep)	      = ILIT(144)
tagOf_PrimOp (ReadByteArrayOp Word64Rep)      = ILIT(145)

tagOf_PrimOp (WriteByteArrayOp CharRep)       = ILIT(146)
tagOf_PrimOp (WriteByteArrayOp IntRep)	      = ILIT(147)
tagOf_PrimOp (WriteByteArrayOp WordRep)	      = ILIT(148)
tagOf_PrimOp (WriteByteArrayOp AddrRep)       = ILIT(149)
tagOf_PrimOp (WriteByteArrayOp FloatRep)      = ILIT(150)
tagOf_PrimOp (WriteByteArrayOp DoubleRep)     = ILIT(151)
tagOf_PrimOp (WriteByteArrayOp StablePtrRep)  = ILIT(152)
tagOf_PrimOp (WriteByteArrayOp Int64Rep)      = ILIT(153)
tagOf_PrimOp (WriteByteArrayOp Word64Rep)     = ILIT(154)

tagOf_PrimOp (IndexByteArrayOp CharRep)       = ILIT(155)
tagOf_PrimOp (IndexByteArrayOp IntRep)	      = ILIT(156)
tagOf_PrimOp (IndexByteArrayOp WordRep)	      = ILIT(157)
tagOf_PrimOp (IndexByteArrayOp AddrRep)       = ILIT(158)
tagOf_PrimOp (IndexByteArrayOp FloatRep)      = ILIT(159)
tagOf_PrimOp (IndexByteArrayOp DoubleRep)     = ILIT(160)
tagOf_PrimOp (IndexByteArrayOp StablePtrRep)  = ILIT(161)
tagOf_PrimOp (IndexByteArrayOp Int64Rep)      = ILIT(162)
tagOf_PrimOp (IndexByteArrayOp Word64Rep)     = ILIT(163)

tagOf_PrimOp (IndexOffAddrOp CharRep)	      = ILIT(164)
tagOf_PrimOp (IndexOffAddrOp IntRep)	      = ILIT(165)
tagOf_PrimOp (IndexOffAddrOp WordRep)	      = ILIT(166)
tagOf_PrimOp (IndexOffAddrOp AddrRep)	      = ILIT(167)
tagOf_PrimOp (IndexOffAddrOp FloatRep)	      = ILIT(168)
tagOf_PrimOp (IndexOffAddrOp DoubleRep)       = ILIT(169)
tagOf_PrimOp (IndexOffAddrOp StablePtrRep)    = ILIT(170)
tagOf_PrimOp (IndexOffAddrOp Int64Rep)	      = ILIT(171)
tagOf_PrimOp (IndexOffAddrOp Word64Rep)	      = ILIT(172)
tagOf_PrimOp (IndexOffForeignObjOp CharRep)   = ILIT(173)
tagOf_PrimOp (IndexOffForeignObjOp IntRep)    = ILIT(174)
tagOf_PrimOp (IndexOffForeignObjOp WordRep)   = ILIT(175)
tagOf_PrimOp (IndexOffForeignObjOp AddrRep)   = ILIT(176)
tagOf_PrimOp (IndexOffForeignObjOp FloatRep)  = ILIT(177)
tagOf_PrimOp (IndexOffForeignObjOp DoubleRep) = ILIT(178)
tagOf_PrimOp (IndexOffForeignObjOp StablePtrRep) = ILIT(179)
tagOf_PrimOp (IndexOffForeignObjOp Int64Rep)  = ILIT(180)
tagOf_PrimOp (IndexOffForeignObjOp Word64Rep) = ILIT(181)

tagOf_PrimOp (WriteOffAddrOp CharRep)         = ILIT(182)
tagOf_PrimOp (WriteOffAddrOp IntRep)          = ILIT(183)
tagOf_PrimOp (WriteOffAddrOp WordRep)         = ILIT(184)
tagOf_PrimOp (WriteOffAddrOp AddrRep)         = ILIT(185)
tagOf_PrimOp (WriteOffAddrOp FloatRep)        = ILIT(186)
tagOf_PrimOp (WriteOffAddrOp DoubleRep)       = ILIT(187)
tagOf_PrimOp (WriteOffAddrOp StablePtrRep)    = ILIT(188)
tagOf_PrimOp (WriteOffAddrOp ForeignObjRep)   = ILIT(189)
tagOf_PrimOp (WriteOffAddrOp Int64Rep)        = ILIT(190)
tagOf_PrimOp (WriteOffAddrOp Word64Rep)       = ILIT(191)

tagOf_PrimOp UnsafeFreezeArrayOp	      = ILIT(192)
tagOf_PrimOp UnsafeFreezeByteArrayOp	      = ILIT(193)
tagOf_PrimOp SizeofByteArrayOp		      = ILIT(194)
tagOf_PrimOp SizeofMutableByteArrayOp	      = ILIT(195)
tagOf_PrimOp NewMVarOp			      = ILIT(196)
tagOf_PrimOp TakeMVarOp		    	      = ILIT(197)
tagOf_PrimOp PutMVarOp		    	      = ILIT(198)
tagOf_PrimOp SameMVarOp		    	      = ILIT(199)
tagOf_PrimOp MakeForeignObjOp		      = ILIT(200)
tagOf_PrimOp WriteForeignObjOp		      = ILIT(201)
tagOf_PrimOp MkWeakOp			      = ILIT(202)
tagOf_PrimOp DeRefWeakOp		      = ILIT(203)
tagOf_PrimOp MakeStablePtrOp		      = ILIT(204)
tagOf_PrimOp DeRefStablePtrOp		      = ILIT(205)
tagOf_PrimOp EqStablePtrOp		      = ILIT(206)
tagOf_PrimOp (CCallOp _ _ _ _)		      = ILIT(207)
tagOf_PrimOp ReallyUnsafePtrEqualityOp	      = ILIT(208)
tagOf_PrimOp SeqOp			      = ILIT(209)
tagOf_PrimOp ParOp			      = ILIT(210)
tagOf_PrimOp ForkOp			      = ILIT(211)
tagOf_PrimOp KillThreadOp		      = ILIT(212)
tagOf_PrimOp DelayOp			      = ILIT(213)
tagOf_PrimOp WaitReadOp			      = ILIT(214)
tagOf_PrimOp WaitWriteOp		      = ILIT(215)
tagOf_PrimOp ParGlobalOp		      = ILIT(216)
tagOf_PrimOp ParLocalOp			      = ILIT(217)
tagOf_PrimOp ParAtOp			      = ILIT(218)
tagOf_PrimOp ParAtAbsOp			      = ILIT(219)
tagOf_PrimOp ParAtRelOp			      = ILIT(220)
tagOf_PrimOp ParAtForNowOp		      = ILIT(221)
tagOf_PrimOp CopyableOp			      = ILIT(222)
tagOf_PrimOp NoFollowOp			      = ILIT(223)
tagOf_PrimOp NewMutVarOp		      = ILIT(224)
tagOf_PrimOp ReadMutVarOp		      = ILIT(225)
tagOf_PrimOp WriteMutVarOp		      = ILIT(226)
tagOf_PrimOp SameMutVarOp		      = ILIT(227)
tagOf_PrimOp CatchOp			      = ILIT(228)
tagOf_PrimOp RaiseOp			      = ILIT(229)

tagOf_PrimOp op = pprPanic# "tagOf_PrimOp: pattern-match" (ppr op)
--panic# "tagOf_PrimOp: pattern-match"

instance Eq PrimOp where
    op1 == op2 = tagOf_PrimOp op1 _EQ_ tagOf_PrimOp op2

instance Ord PrimOp where
    op1 <  op2 =  tagOf_PrimOp op1 _LT_ tagOf_PrimOp op2
    op1 <= op2 =  tagOf_PrimOp op1 _LE_ tagOf_PrimOp op2
    op1 >= op2 =  tagOf_PrimOp op1 _GE_ tagOf_PrimOp op2
    op1 >  op2 =  tagOf_PrimOp op1 _GT_ tagOf_PrimOp op2
    op1 `compare` op2 | op1 < op2  = LT
		      | op1 == op2 = EQ
		      | otherwise  = GT

instance Outputable PrimOp where
    ppr op = pprPrimOp op

instance Show PrimOp where
    showsPrec p op = showsPrecSDoc p (pprPrimOp op)
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
	WordQuotOp,
	WordRemOp,
	AndOp,
	OrOp,
	NotOp,
	XorOp,
    	SllOp,
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
	IntegerGcdOp,
	IntegerQuotRemOp,
	IntegerDivModOp,
	IntegerNegOp,
	IntegerCmpOp,
	Integer2IntOp,
	Integer2WordOp,
	Int2IntegerOp,
	Word2IntegerOp,
	Addr2IntegerOp,
	IntegerToInt64Op,
	Int64ToIntegerOp,
	IntegerToWord64Op,
	Word64ToIntegerOp,
	FloatEncodeOp,
	FloatDecodeOp,
	DoubleEncodeOp,
	DoubleDecodeOp,
	NewArrayOp,
	NewByteArrayOp CharRep,
	NewByteArrayOp IntRep,
	NewByteArrayOp WordRep,
	NewByteArrayOp AddrRep,
	NewByteArrayOp FloatRep,
	NewByteArrayOp DoubleRep,
	NewByteArrayOp StablePtrRep,
	SameMutableArrayOp,
	SameMutableByteArrayOp,
	ReadArrayOp,
	WriteArrayOp,
	IndexArrayOp,
	ReadByteArrayOp CharRep,
	ReadByteArrayOp IntRep,
	ReadByteArrayOp WordRep,
	ReadByteArrayOp AddrRep,
	ReadByteArrayOp FloatRep,
	ReadByteArrayOp DoubleRep,
	ReadByteArrayOp StablePtrRep,
	ReadByteArrayOp Int64Rep,
	ReadByteArrayOp Word64Rep,
	WriteByteArrayOp CharRep,
	WriteByteArrayOp IntRep,
	WriteByteArrayOp WordRep,
	WriteByteArrayOp AddrRep,
	WriteByteArrayOp FloatRep,
	WriteByteArrayOp DoubleRep,
	WriteByteArrayOp StablePtrRep,
	WriteByteArrayOp Int64Rep,
	WriteByteArrayOp Word64Rep,
	IndexByteArrayOp CharRep,
	IndexByteArrayOp IntRep,
	IndexByteArrayOp WordRep,
	IndexByteArrayOp AddrRep,
	IndexByteArrayOp FloatRep,
	IndexByteArrayOp DoubleRep,
	IndexByteArrayOp StablePtrRep,
	IndexByteArrayOp Int64Rep,
	IndexByteArrayOp Word64Rep,
	IndexOffForeignObjOp CharRep,
	IndexOffForeignObjOp AddrRep,
	IndexOffForeignObjOp IntRep,
	IndexOffForeignObjOp WordRep,
	IndexOffForeignObjOp FloatRep,
	IndexOffForeignObjOp DoubleRep,
	IndexOffForeignObjOp StablePtrRep,
	IndexOffForeignObjOp Int64Rep,
	IndexOffForeignObjOp Word64Rep,
	IndexOffAddrOp CharRep,
	IndexOffAddrOp IntRep,
	IndexOffAddrOp WordRep,
	IndexOffAddrOp AddrRep,
	IndexOffAddrOp FloatRep,
	IndexOffAddrOp DoubleRep,
	IndexOffAddrOp StablePtrRep,
	IndexOffAddrOp Int64Rep,
	IndexOffAddrOp Word64Rep,
	WriteOffAddrOp CharRep,
	WriteOffAddrOp IntRep,
	WriteOffAddrOp WordRep,
	WriteOffAddrOp AddrRep,
	WriteOffAddrOp FloatRep,
	WriteOffAddrOp DoubleRep,
	WriteOffAddrOp ForeignObjRep,
	WriteOffAddrOp StablePtrRep,
	WriteOffAddrOp Int64Rep,
	WriteOffAddrOp Word64Rep,
	UnsafeFreezeArrayOp,
	UnsafeFreezeByteArrayOp,
	SizeofByteArrayOp,
	SizeofMutableByteArrayOp,
	NewMutVarOp,
	ReadMutVarOp,
	WriteMutVarOp,
	SameMutVarOp,
        CatchOp,
        RaiseOp,
    	NewMVarOp,
	TakeMVarOp,
	PutMVarOp,
	SameMVarOp,
	MakeForeignObjOp,
	WriteForeignObjOp,
	MkWeakOp,
	DeRefWeakOp,
	MakeStablePtrOp,
	DeRefStablePtrOp,
	EqStablePtrOp,
	ReallyUnsafePtrEqualityOp,
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
	KillThreadOp,
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
  = Dyadic	OccName		-- string :: T -> T -> T
		Type
  | Monadic	OccName		-- string :: T -> T
		Type
  | Compare	OccName		-- string :: T -> T -> Bool
		Type

  | GenPrimOp   OccName  	-- string :: \/a1..an . T1 -> .. -> Tk -> T
		[TyVar] 
		[Type] 
		Type 

mkDyadic str  ty = Dyadic  (varOcc str) ty
mkMonadic str ty = Monadic (varOcc str) ty
mkCompare str ty = Compare (varOcc str) ty
mkGenPrimOp str tvs tys ty = GenPrimOp (varOcc str) tvs tys ty
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

unboxedPair	 = mkUnboxedTupleTy 2
unboxedTriple    = mkUnboxedTupleTy 3
unboxedQuadruple = mkUnboxedTupleTy 4
unboxedSexTuple  = mkUnboxedTupleTy 6

integerMonadic name = mkGenPrimOp name [] one_Integer_ty 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

integerDyadic name = mkGenPrimOp name [] two_Integer_tys 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

integerDyadic2Results name = mkGenPrimOp name [] two_Integer_tys 
    (unboxedSexTuple [intPrimTy, intPrimTy, byteArrayPrimTy, 
	 	      intPrimTy, intPrimTy, byteArrayPrimTy])

integerCompare name = mkGenPrimOp name [] two_Integer_tys intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection{Strictness}
%*									*
%************************************************************************

Not all primops are strict!

\begin{code}
primOpStrictness :: PrimOp -> ([Demand], Bool)
	-- See IdInfo.StrictnessInfo for discussion of what the results
	-- **NB** as a cheap hack, to avoid having to look up the PrimOp's arity,
	-- the list of demands may be infinite!
	-- Use only the ones you ned.

primOpStrictness SeqOp            = ([wwLazy], False)
primOpStrictness NewArrayOp       = ([wwPrim, wwLazy, wwPrim], False)
primOpStrictness WriteArrayOp     = ([wwPrim, wwPrim, wwLazy, wwPrim], False)

primOpStrictness NewMutVarOp	  = ([wwLazy, wwPrim], False)
primOpStrictness WriteMutVarOp	  = ([wwPrim, wwLazy, wwPrim], False)

primOpStrictness PutMVarOp	  = ([wwPrim, wwLazy, wwPrim], False)

primOpStrictness CatchOp	  = ([wwLazy, wwLazy], False)
primOpStrictness RaiseOp	  = ([wwLazy], True)	-- NB: True => result is bottom

primOpStrictness MkWeakOp	  = ([wwLazy, wwLazy, wwLazy, wwPrim], False)
primOpStrictness MakeStablePtrOp  = ([wwLazy, wwPrim], False)

	-- The rest all have primitive-typed arguments
primOpStrictness other		  = (repeat wwPrim, False)
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
%*									*
%************************************************************************

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.

\begin{code}
primOpInfo :: PrimOp -> PrimOpInfo
\end{code}

There's plenty of this stuff!

\begin{code}
primOpInfo CharGtOp   = mkCompare SLIT("gtChar#")   charPrimTy
primOpInfo CharGeOp   = mkCompare SLIT("geChar#")   charPrimTy
primOpInfo CharEqOp   = mkCompare SLIT("eqChar#")   charPrimTy
primOpInfo CharNeOp   = mkCompare SLIT("neChar#")   charPrimTy
primOpInfo CharLtOp   = mkCompare SLIT("ltChar#")   charPrimTy
primOpInfo CharLeOp   = mkCompare SLIT("leChar#")   charPrimTy

primOpInfo IntGtOp    = mkCompare SLIT(">#")	   intPrimTy
primOpInfo IntGeOp    = mkCompare SLIT(">=#")	   intPrimTy
primOpInfo IntEqOp    = mkCompare SLIT("==#")	   intPrimTy
primOpInfo IntNeOp    = mkCompare SLIT("/=#")	   intPrimTy
primOpInfo IntLtOp    = mkCompare SLIT("<#")	   intPrimTy
primOpInfo IntLeOp    = mkCompare SLIT("<=#")	   intPrimTy

primOpInfo WordGtOp   = mkCompare SLIT("gtWord#")   wordPrimTy
primOpInfo WordGeOp   = mkCompare SLIT("geWord#")   wordPrimTy
primOpInfo WordEqOp   = mkCompare SLIT("eqWord#")   wordPrimTy
primOpInfo WordNeOp   = mkCompare SLIT("neWord#")   wordPrimTy
primOpInfo WordLtOp   = mkCompare SLIT("ltWord#")   wordPrimTy
primOpInfo WordLeOp   = mkCompare SLIT("leWord#")   wordPrimTy

primOpInfo AddrGtOp   = mkCompare SLIT("gtAddr#")   addrPrimTy
primOpInfo AddrGeOp   = mkCompare SLIT("geAddr#")   addrPrimTy
primOpInfo AddrEqOp   = mkCompare SLIT("eqAddr#")   addrPrimTy
primOpInfo AddrNeOp   = mkCompare SLIT("neAddr#")   addrPrimTy
primOpInfo AddrLtOp   = mkCompare SLIT("ltAddr#")   addrPrimTy
primOpInfo AddrLeOp   = mkCompare SLIT("leAddr#")   addrPrimTy

primOpInfo FloatGtOp  = mkCompare SLIT("gtFloat#")  floatPrimTy
primOpInfo FloatGeOp  = mkCompare SLIT("geFloat#")  floatPrimTy
primOpInfo FloatEqOp  = mkCompare SLIT("eqFloat#")  floatPrimTy
primOpInfo FloatNeOp  = mkCompare SLIT("neFloat#")  floatPrimTy
primOpInfo FloatLtOp  = mkCompare SLIT("ltFloat#")  floatPrimTy
primOpInfo FloatLeOp  = mkCompare SLIT("leFloat#")  floatPrimTy

primOpInfo DoubleGtOp = mkCompare SLIT(">##") doublePrimTy
primOpInfo DoubleGeOp = mkCompare SLIT(">=##") doublePrimTy
primOpInfo DoubleEqOp = mkCompare SLIT("==##") doublePrimTy
primOpInfo DoubleNeOp = mkCompare SLIT("/=##") doublePrimTy
primOpInfo DoubleLtOp = mkCompare SLIT("<##") doublePrimTy
primOpInfo DoubleLeOp = mkCompare SLIT("<=##") doublePrimTy

\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Char]{PrimOpInfo for @Char#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo OrdOp = mkGenPrimOp SLIT("ord#") [] [charPrimTy] intPrimTy
primOpInfo ChrOp = mkGenPrimOp SLIT("chr#") [] [intPrimTy]  charPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Int]{PrimOpInfo for @Int#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo IntAddOp  = mkDyadic SLIT("+#")	 intPrimTy
primOpInfo IntSubOp  = mkDyadic SLIT("-#") intPrimTy
primOpInfo IntMulOp  = mkDyadic SLIT("*#") intPrimTy
primOpInfo IntQuotOp = mkDyadic SLIT("quotInt#")	 intPrimTy
primOpInfo IntRemOp  = mkDyadic SLIT("remInt#")	 intPrimTy

primOpInfo IntNegOp  = mkMonadic SLIT("negateInt#") intPrimTy
primOpInfo IntAbsOp  = mkMonadic SLIT("absInt#") intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Word]{PrimOpInfo for @Word#@s}
%*									*
%************************************************************************

A @Word#@ is an unsigned @Int#@.

\begin{code}
primOpInfo WordQuotOp = mkDyadic SLIT("quotWord#") wordPrimTy
primOpInfo WordRemOp  = mkDyadic SLIT("remWord#")	 wordPrimTy

primOpInfo AndOp    = mkDyadic  SLIT("and#")	wordPrimTy
primOpInfo OrOp	    = mkDyadic  SLIT("or#")	wordPrimTy
primOpInfo XorOp    = mkDyadic  SLIT("xor#")	wordPrimTy
primOpInfo NotOp    = mkMonadic SLIT("not#")	wordPrimTy

primOpInfo SllOp
  = mkGenPrimOp SLIT("shiftL#")  [] [wordPrimTy, intPrimTy] wordPrimTy
primOpInfo SrlOp
  = mkGenPrimOp SLIT("shiftRL#") [] [wordPrimTy, intPrimTy] wordPrimTy

primOpInfo ISllOp
  = mkGenPrimOp SLIT("iShiftL#")  [] [intPrimTy, intPrimTy] intPrimTy
primOpInfo ISraOp
  = mkGenPrimOp SLIT("iShiftRA#") [] [intPrimTy, intPrimTy] intPrimTy
primOpInfo ISrlOp
  = mkGenPrimOp SLIT("iShiftRL#") [] [intPrimTy, intPrimTy] intPrimTy

primOpInfo Int2WordOp = mkGenPrimOp SLIT("int2Word#") [] [intPrimTy] wordPrimTy
primOpInfo Word2IntOp = mkGenPrimOp SLIT("word2Int#") [] [wordPrimTy] intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Addr]{PrimOpInfo for @Addr#@s}
%*									*
%************************************************************************

\begin{code}
primOpInfo Int2AddrOp = mkGenPrimOp SLIT("int2Addr#") [] [intPrimTy] addrPrimTy
primOpInfo Addr2IntOp = mkGenPrimOp SLIT("addr2Int#") [] [addrPrimTy] intPrimTy
\end{code}


%************************************************************************
%*									*
\subsubsection[PrimOp-Float]{PrimOpInfo for @Float#@s}
%*									*
%************************************************************************

@encodeFloat#@ and @decodeFloat#@ are given w/ Integer-stuff (it's
similar).

\begin{code}
primOpInfo FloatAddOp	= mkDyadic    SLIT("plusFloat#")	   floatPrimTy
primOpInfo FloatSubOp	= mkDyadic    SLIT("minusFloat#")   floatPrimTy
primOpInfo FloatMulOp	= mkDyadic    SLIT("timesFloat#")   floatPrimTy
primOpInfo FloatDivOp	= mkDyadic    SLIT("divideFloat#")  floatPrimTy
primOpInfo FloatNegOp	= mkMonadic   SLIT("negateFloat#")  floatPrimTy

primOpInfo Float2IntOp	= mkGenPrimOp SLIT("float2Int#") [] [floatPrimTy] intPrimTy
primOpInfo Int2FloatOp	= mkGenPrimOp SLIT("int2Float#") [] [intPrimTy] floatPrimTy

primOpInfo FloatExpOp	= mkMonadic   SLIT("expFloat#")	   floatPrimTy
primOpInfo FloatLogOp	= mkMonadic   SLIT("logFloat#")	   floatPrimTy
primOpInfo FloatSqrtOp	= mkMonadic   SLIT("sqrtFloat#")	   floatPrimTy
primOpInfo FloatSinOp	= mkMonadic   SLIT("sinFloat#")	   floatPrimTy
primOpInfo FloatCosOp	= mkMonadic   SLIT("cosFloat#")	   floatPrimTy
primOpInfo FloatTanOp	= mkMonadic   SLIT("tanFloat#")	   floatPrimTy
primOpInfo FloatAsinOp	= mkMonadic   SLIT("asinFloat#")	   floatPrimTy
primOpInfo FloatAcosOp	= mkMonadic   SLIT("acosFloat#")	   floatPrimTy
primOpInfo FloatAtanOp	= mkMonadic   SLIT("atanFloat#")	   floatPrimTy
primOpInfo FloatSinhOp	= mkMonadic   SLIT("sinhFloat#")	   floatPrimTy
primOpInfo FloatCoshOp	= mkMonadic   SLIT("coshFloat#")	   floatPrimTy
primOpInfo FloatTanhOp	= mkMonadic   SLIT("tanhFloat#")	   floatPrimTy
primOpInfo FloatPowerOp	= mkDyadic    SLIT("powerFloat#")   floatPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Double]{PrimOpInfo for @Double#@s}
%*									*
%************************************************************************

@encodeDouble#@ and @decodeDouble#@ are given w/ Integer-stuff (it's
similar).

\begin{code}
primOpInfo DoubleAddOp	= mkDyadic    SLIT("+##")   doublePrimTy
primOpInfo DoubleSubOp	= mkDyadic    SLIT("-##")  doublePrimTy
primOpInfo DoubleMulOp	= mkDyadic    SLIT("*##")  doublePrimTy
primOpInfo DoubleDivOp	= mkDyadic    SLIT("/##") doublePrimTy
primOpInfo DoubleNegOp	= mkMonadic   SLIT("negateDouble#") doublePrimTy

primOpInfo Double2IntOp	    = mkGenPrimOp SLIT("double2Int#") [] [doublePrimTy] intPrimTy
primOpInfo Int2DoubleOp	    = mkGenPrimOp SLIT("int2Double#") [] [intPrimTy] doublePrimTy

primOpInfo Double2FloatOp   = mkGenPrimOp SLIT("double2Float#") [] [doublePrimTy] floatPrimTy
primOpInfo Float2DoubleOp   = mkGenPrimOp SLIT("float2Double#") [] [floatPrimTy] doublePrimTy

primOpInfo DoubleExpOp	= mkMonadic   SLIT("expDouble#")	   doublePrimTy
primOpInfo DoubleLogOp	= mkMonadic   SLIT("logDouble#")	   doublePrimTy
primOpInfo DoubleSqrtOp	= mkMonadic   SLIT("sqrtDouble#")   doublePrimTy
primOpInfo DoubleSinOp	= mkMonadic   SLIT("sinDouble#")	   doublePrimTy
primOpInfo DoubleCosOp	= mkMonadic   SLIT("cosDouble#")	   doublePrimTy
primOpInfo DoubleTanOp	= mkMonadic   SLIT("tanDouble#")	   doublePrimTy
primOpInfo DoubleAsinOp	= mkMonadic   SLIT("asinDouble#")   doublePrimTy
primOpInfo DoubleAcosOp	= mkMonadic   SLIT("acosDouble#")   doublePrimTy
primOpInfo DoubleAtanOp	= mkMonadic   SLIT("atanDouble#")   doublePrimTy
primOpInfo DoubleSinhOp	= mkMonadic   SLIT("sinhDouble#")   doublePrimTy
primOpInfo DoubleCoshOp	= mkMonadic   SLIT("coshDouble#")   doublePrimTy
primOpInfo DoubleTanhOp	= mkMonadic   SLIT("tanhDouble#")   doublePrimTy
primOpInfo DoublePowerOp= mkDyadic    SLIT("**##")  doublePrimTy
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
primOpInfo IntegerGcdOp	= integerDyadic SLIT("gcdInteger#")

primOpInfo IntegerCmpOp	= integerCompare SLIT("cmpInteger#")

primOpInfo IntegerQuotRemOp = integerDyadic2Results SLIT("quotRemInteger#")
primOpInfo IntegerDivModOp  = integerDyadic2Results SLIT("divModInteger#")

primOpInfo Integer2IntOp
  = mkGenPrimOp SLIT("integer2Int#") [] one_Integer_ty intPrimTy

primOpInfo Integer2WordOp
  = mkGenPrimOp SLIT("integer2Word#") [] one_Integer_ty wordPrimTy

primOpInfo Int2IntegerOp
  = mkGenPrimOp SLIT("int2Integer#") [] [intPrimTy] 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

primOpInfo Word2IntegerOp
  = mkGenPrimOp SLIT("word2Integer#") [] [wordPrimTy] 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

primOpInfo Addr2IntegerOp
  = mkGenPrimOp SLIT("addr2Integer#") [] [addrPrimTy] 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

primOpInfo IntegerToInt64Op
  = mkGenPrimOp SLIT("integerToInt64#") [] one_Integer_ty int64PrimTy

primOpInfo Int64ToIntegerOp
  = mkGenPrimOp SLIT("int64ToInteger#") [] [int64PrimTy]
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

primOpInfo Word64ToIntegerOp
  = mkGenPrimOp SLIT("word64ToInteger#") [] [word64PrimTy] 
			(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])

primOpInfo IntegerToWord64Op
  = mkGenPrimOp SLIT("integerToWord64#") [] one_Integer_ty word64PrimTy
\end{code}

Encoding and decoding of floating-point numbers is sorta
Integer-related.

\begin{code}
primOpInfo FloatEncodeOp
  = mkGenPrimOp SLIT("encodeFloat#") [] an_Integer_and_Int_tys floatPrimTy

primOpInfo DoubleEncodeOp
  = mkGenPrimOp SLIT("encodeDouble#") [] an_Integer_and_Int_tys doublePrimTy

primOpInfo FloatDecodeOp
  = mkGenPrimOp SLIT("decodeFloat#") [] [floatPrimTy] 
	(unboxedQuadruple [intPrimTy, intPrimTy, intPrimTy, byteArrayPrimTy])
primOpInfo DoubleDecodeOp
  = mkGenPrimOp SLIT("decodeDouble#") [] [doublePrimTy] 
	(unboxedQuadruple [intPrimTy, intPrimTy, intPrimTy, byteArrayPrimTy])
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Arrays]{PrimOpInfo for primitive arrays}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("newArray#") [s_tv, elt_tv] 
	[intPrimTy, elt, state]
	(unboxedPair [state, mkMutableArrayPrimTy s elt])

primOpInfo (NewByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	op_str	       = _PK_ ("new" ++ primRepString kind ++ "Array#")
	state = mkStatePrimTy s
    in
    mkGenPrimOp op_str [s_tv]
	[intPrimTy, state]
	(unboxedPair [state, mkMutableByteArrayPrimTy s])

---------------------------------------------------------------------------

primOpInfo SameMutableArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	mut_arr_ty = mkMutableArrayPrimTy s elt
    } in
    mkGenPrimOp SLIT("sameMutableArray#") [s_tv, elt_tv] [mut_arr_ty, mut_arr_ty]
				   boolTy

primOpInfo SameMutableByteArrayOp
  = let {
	s = alphaTy; s_tv = alphaTyVar;
	mut_arr_ty = mkMutableByteArrayPrimTy s
    } in
    mkGenPrimOp SLIT("sameMutableByteArray#") [s_tv] [mut_arr_ty, mut_arr_ty]
				   boolTy

---------------------------------------------------------------------------
-- Primitive arrays of Haskell pointers:

primOpInfo ReadArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("readArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, state]
	(unboxedPair [state, elt])


primOpInfo WriteArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    mkGenPrimOp SLIT("writeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, intPrimTy, elt, mkStatePrimTy s]
	(mkStatePrimTy s)

primOpInfo IndexArrayOp
  = let { elt = alphaTy; elt_tv = alphaTyVar } in
    mkGenPrimOp SLIT("indexArray#") [elt_tv] [mkArrayPrimTy elt, intPrimTy]
	(unboxedPair [realWorldStatePrimTy, elt])

---------------------------------------------------------------------------
-- Primitive arrays full of unboxed bytes:

primOpInfo (ReadByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	op_str	       = _PK_ ("read" ++ primRepString kind ++ "Array#")
	relevant_type  = assoc "primOpInfo{ReadByteArrayOp}" tbl kind
	state          = mkStatePrimTy s

        tvs
	  | kind == StablePtrRep = [s_tv, betaTyVar]
	  | otherwise	         = [s_tv]
    in
    mkGenPrimOp op_str tvs
	[mkMutableByteArrayPrimTy s, intPrimTy, state]
	(unboxedPair [state, relevant_type])
  where
    tbl = [ (CharRep,	 charPrimTy),
	    (IntRep,	 intPrimTy),
	    (WordRep,	 wordPrimTy),
	    (AddrRep,	 addrPrimTy),
	    (FloatRep,	 floatPrimTy),
	    (StablePtrRep, mkStablePtrPrimTy betaTy),
	    (DoubleRep,  doublePrimTy) ]

  -- How come there's no Word byte arrays? ADR

primOpInfo (WriteByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar
	op_str = _PK_ ("write" ++ primRepString kind ++ "Array#")
	prim_ty = mkTyConApp (primRepTyCon kind) []

        (the_prim_ty, tvs)
	  | kind == StablePtrRep = (mkStablePtrPrimTy betaTy, [s_tv, betaTyVar])
	  | otherwise		 = (prim_ty, [s_tv])

    in
    mkGenPrimOp op_str tvs
	[mkMutableByteArrayPrimTy s, intPrimTy, the_prim_ty, mkStatePrimTy s]
	(mkStatePrimTy s)

primOpInfo (IndexByteArrayOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "Array#")

        (prim_tycon_args, tvs)
	  | kind == StablePtrRep = ([alphaTy], [alphaTyVar])
	  | otherwise	         = ([],[])
    in
    mkGenPrimOp op_str tvs [byteArrayPrimTy, intPrimTy] 
	(mkTyConApp (primRepTyCon kind) prim_tycon_args)

primOpInfo (IndexOffForeignObjOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "OffForeignObj#")

        (prim_tycon_args, tvs)
	  | kind == StablePtrRep = ([alphaTy], [alphaTyVar])
	  | otherwise	         = ([], [])
    in
    mkGenPrimOp op_str tvs [foreignObjPrimTy, intPrimTy] 
	(mkTyConApp (primRepTyCon kind) prim_tycon_args)

primOpInfo (IndexOffAddrOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "OffAddr#")

        (prim_tycon_args, tvs)
	  | kind == StablePtrRep = ([alphaTy], [alphaTyVar])
	  | otherwise	         = ([], [])
    in
    mkGenPrimOp op_str tvs [addrPrimTy, intPrimTy] 
	(mkTyConApp (primRepTyCon kind) prim_tycon_args)

primOpInfo (WriteOffAddrOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar
	op_str = _PK_ ("write" ++ primRepString kind ++ "OffAddr#")
	prim_ty = mkTyConApp (primRepTyCon kind) []
    in
    mkGenPrimOp op_str [s_tv]
	[addrPrimTy, intPrimTy, prim_ty, mkStatePrimTy s]
	(mkStatePrimTy s)

---------------------------------------------------------------------------
primOpInfo UnsafeFreezeArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("unsafeFreezeArray#") [s_tv, elt_tv]
	[mkMutableArrayPrimTy s elt, state]
	(unboxedPair [state, mkArrayPrimTy elt])

primOpInfo UnsafeFreezeByteArrayOp
  = let { 
	s = alphaTy; s_tv = alphaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("unsafeFreezeByteArray#") [s_tv]
	[mkMutableByteArrayPrimTy s, state]
	(unboxedPair [state, byteArrayPrimTy])

---------------------------------------------------------------------------
primOpInfo SizeofByteArrayOp
  = mkGenPrimOp
        SLIT("sizeofByteArray#") []
	[byteArrayPrimTy]
        intPrimTy

primOpInfo SizeofMutableByteArrayOp
  = let { s = alphaTy; s_tv = alphaTyVar } in
    mkGenPrimOp
        SLIT("sizeofMutableByteArray#") [s_tv]
	[mkMutableByteArrayPrimTy s]
        intPrimTy
\end{code}


%************************************************************************
%*									*
\subsubsection[PrimOp-MutVars]{PrimOpInfo for mutable variable ops}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewMutVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("newMutVar#") [s_tv, elt_tv] 
	[elt, state]
	(unboxedPair [state, mkMutVarPrimTy s elt])

primOpInfo ReadMutVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("readMutVar#") [s_tv, elt_tv]
	[mkMutVarPrimTy s elt, state]
	(unboxedPair [state, elt])


primOpInfo WriteMutVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    } in
    mkGenPrimOp SLIT("writeMutVar#") [s_tv, elt_tv]
	[mkMutVarPrimTy s elt, elt, mkStatePrimTy s]
	(mkStatePrimTy s)

primOpInfo SameMutVarOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	mut_var_ty = mkMutVarPrimTy s elt
    } in
    mkGenPrimOp SLIT("sameMutVar#") [s_tv, elt_tv] [mut_var_ty, mut_var_ty]
				   boolTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Exceptions]{PrimOpInfo for exceptions}
%*									*
%************************************************************************

catch :: IO a -> (IOError -> IO a) -> IO a
catch :: a  -> (b -> a) -> a

\begin{code}
primOpInfo CatchOp   
  = let
	a = alphaTy; a_tv = alphaTyVar;
	b = betaTy;  b_tv = betaTyVar;
    in
    mkGenPrimOp SLIT("catch#") [a_tv, b_tv] [a, mkFunTy b a] a

primOpInfo RaiseOp
  = let
	a = alphaTy; a_tv = alphaTyVar;
	b = betaTy;  b_tv = betaTyVar;
    in
    mkGenPrimOp SLIT("raise#") [a_tv, b_tv] [a] b
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-MVars]{PrimOpInfo for synchronizing Variables}
%*									*
%************************************************************************

\begin{code}
primOpInfo NewMVarOp
  = let
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
	state = mkStatePrimTy s
    in
    mkGenPrimOp SLIT("newMVar#") [s_tv, elt_tv] [state]
	(unboxedPair [state, mkMVarPrimTy s elt])

primOpInfo TakeMVarOp
  = let
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
	state = mkStatePrimTy s
    in
    mkGenPrimOp SLIT("takeMVar#") [s_tv, elt_tv]
	[mkMVarPrimTy s elt, state]
	(unboxedPair [state, elt])

primOpInfo PutMVarOp
  = let
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
    in
    mkGenPrimOp SLIT("putMVar#") [s_tv, elt_tv]
	[mkMVarPrimTy s elt, elt, mkStatePrimTy s]
	(mkStatePrimTy s)

primOpInfo SameMVarOp
  = let
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
	mvar_ty = mkMVarPrimTy s elt
    in
    mkGenPrimOp SLIT("sameMVar#") [s_tv, elt_tv] [mvar_ty, mvar_ty] boolTy
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
    mkGenPrimOp SLIT("delay#") [s_tv]
	[intPrimTy, mkStatePrimTy s] (mkStatePrimTy s)

primOpInfo WaitReadOp
  = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
    mkGenPrimOp SLIT("waitRead#") [s_tv]
	[intPrimTy, mkStatePrimTy s] (mkStatePrimTy s)

primOpInfo WaitWriteOp
  = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
    mkGenPrimOp SLIT("waitWrite#") [s_tv]
	[intPrimTy, mkStatePrimTy s] (mkStatePrimTy s)
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Concurrency]{Concurrency Primitives}
%*									*
%************************************************************************

\begin{code}
-- fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
primOpInfo ForkOp	
  = mkGenPrimOp SLIT("fork#") [alphaTyVar] 
	[alphaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, threadIdPrimTy])

-- killThread# :: ThreadId# -> State# RealWorld -> State# RealWorld
primOpInfo KillThreadOp
  = mkGenPrimOp SLIT("killThread#") [] 
	[threadIdPrimTy, realWorldStatePrimTy]
	realWorldStatePrimTy
\end{code}

************************************************************************
%*									*
\subsubsection[PrimOps-Foreign]{PrimOpInfo for Foreign Objects}
%*									*
%************************************************************************

\begin{code}
primOpInfo MakeForeignObjOp
  = mkGenPrimOp SLIT("makeForeignObj#") [] 
	[addrPrimTy, realWorldStatePrimTy] 
	(unboxedPair [realWorldStatePrimTy, foreignObjPrimTy])

primOpInfo WriteForeignObjOp
 = let {
	s = alphaTy; s_tv = alphaTyVar
    } in
   mkGenPrimOp SLIT("writeForeignObj#") [s_tv]
	[foreignObjPrimTy, addrPrimTy, mkStatePrimTy s] (mkStatePrimTy s)
\end{code}

************************************************************************
%*									*
\subsubsection[PrimOps-Weak]{PrimOpInfo for Weak Pointers}
%*									*
%************************************************************************

A @Weak@ Pointer is created by the @mkWeak#@ primitive:

	mkWeak# :: k -> v -> f -> State# RealWorld 
			-> (# State# RealWorld, Weak# v #)

In practice, you'll use the higher-level

	data Weak v = Weak# v
	mkWeak :: k -> v -> IO () -> IO (Weak v)

\begin{code}
primOpInfo MkWeakOp
  = mkGenPrimOp SLIT("mkWeak#") [alphaTyVar, betaTyVar, gammaTyVar] 
	[alphaTy, betaTy, gammaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, mkWeakPrimTy betaTy])
\end{code}

The following operation dereferences a weak pointer.  The weak pointer
may have been finalised, so the operation returns a result code which
must be inspected before looking at the dereferenced value.

	deRefWeak# :: Weak# v -> State# RealWorld ->
			(# State# RealWorld, v, Int# #)

Only look at v if the Int# returned is /= 0 !!

The higher-level op is

	deRefWeak :: Weak v -> IO (Maybe v)

\begin{code}
primOpInfo DeRefWeakOp
 = mkGenPrimOp SLIT("deRefWeak#") [alphaTyVar]
	[mkWeakPrimTy alphaTy, realWorldStatePrimTy]
	(unboxedTriple [realWorldStatePrimTy, intPrimTy, alphaTy])
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
makeStablePtr#  :: a -> State# _RealWorld -> (# State# _RealWorld, a #)
freeStablePtr#  :: StablePtr# a -> State# _RealWorld -> State# _RealWorld
deRefStablePtr# :: StablePtr# a -> State# _RealWorld -> (# State# _RealWorld, a #)
eqStablePtr#    :: StablePtr# a -> StablePtr# a -> Int#
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
  = mkGenPrimOp SLIT("makeStablePtr#") [alphaTyVar]
	[alphaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, 
			mkTyConApp stablePtrPrimTyCon [alphaTy]])

primOpInfo DeRefStablePtrOp
  = mkGenPrimOp SLIT("deRefStablePtr#") [alphaTyVar]
	[mkStablePtrPrimTy alphaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, alphaTy])

primOpInfo EqStablePtrOp
  = mkGenPrimOp SLIT("eqStablePtr#") [alphaTyVar, betaTyVar]
	[mkStablePtrPrimTy alphaTy, mkStablePtrPrimTy betaTy]
	intPrimTy
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
  = mkGenPrimOp SLIT("reallyUnsafePtrEquality#") [alphaTyVar]
	[alphaTy, alphaTy] intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-parallel]{PrimOpInfo for parallelism op(s)}
%*									*
%************************************************************************

\begin{code}
primOpInfo SeqOp	-- seq# :: a -> Int#
  = mkGenPrimOp SLIT("seq#")	[alphaTyVar] [alphaTy] intPrimTy

primOpInfo ParOp	-- par# :: a -> Int#
  = mkGenPrimOp SLIT("par#")	[alphaTyVar] [alphaTy] intPrimTy
\end{code}

\begin{code}
-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#

primOpInfo ParGlobalOp	-- parGlobal# :: Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = mkGenPrimOp SLIT("parGlobal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParLocalOp	-- parLocal# :: Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = mkGenPrimOp SLIT("parLocal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtOp	-- parAt# :: Int# -> Int# -> Int# -> Int# -> a -> b -> c -> c
  = mkGenPrimOp SLIT("parAt#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTy

primOpInfo ParAtAbsOp	-- parAtAbs# :: Int# -> Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = mkGenPrimOp SLIT("parAtAbs#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtRelOp	-- parAtRel# :: Int# -> Int# -> Int# -> Int# -> Int# -> a -> b -> b
  = mkGenPrimOp SLIT("parAtRel#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtForNowOp	-- parAtForNow# :: Int# -> Int# -> Int# -> Int# -> a -> b -> c -> c
  = mkGenPrimOp SLIT("parAtForNow#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTy

primOpInfo CopyableOp	-- copyable# :: a -> a
  = mkGenPrimOp SLIT("copyable#")	[alphaTyVar] [alphaTy] intPrimTy

primOpInfo NoFollowOp	-- noFollow# :: a -> a
  = mkGenPrimOp SLIT("noFollow#")	[alphaTyVar] [alphaTy] intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-IO-etc]{PrimOpInfo for C calls, and I/O-ish things}
%*									*
%************************************************************************

\begin{code}
primOpInfo (CCallOp _ _ _ _)
     = mkGenPrimOp SLIT("ccall#") [alphaTyVar] [] alphaTy

{-
primOpInfo (CCallOp _ _ _ _ arg_tys result_ty)
  = mkGenPrimOp SLIT("ccall#") [] arg_tys result_tycon tys_applied
  where
    (result_tycon, tys_applied, _) = splitAlgTyConApp result_ty
-}
#ifdef DEBUG
primOpInfo op = panic ("primOpInfo:"++ show (I# (tagOf_PrimOp op)))
#endif
\end{code}

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.

\begin{code}
primOpOutOfLine op
  = case op of
    	TakeMVarOp    		-> True
	PutMVarOp     		-> True
	DelayOp       		-> True
	WaitReadOp    		-> True
	WaitWriteOp   		-> True
	CatchOp	      		-> True
	RaiseOp	      		-> True
	NewArrayOp    		-> True
	NewByteArrayOp _ 	-> True
	IntegerAddOp    	-> True
	IntegerSubOp    	-> True
	IntegerMulOp    	-> True
	IntegerGcdOp    	-> True
	IntegerQuotRemOp    	-> True
	IntegerDivModOp    	-> True
	Int2IntegerOp		-> True
	Word2IntegerOp  	-> True
	Addr2IntegerOp		-> True
	Word64ToIntegerOp       -> True
	Int64ToIntegerOp        -> True
	FloatDecodeOp		-> True
	DoubleDecodeOp		-> True
	MkWeakOp		-> True
	DeRefWeakOp		-> True
	MakeForeignObjOp	-> True
	MakeStablePtrOp		-> True
	NewMutVarOp		-> True
	NewMVarOp		-> True
	ForkOp			-> True
	KillThreadOp		-> True
	CCallOp _ _ may_gc@True _ -> True	-- _ccall_GC_
	_           		-> False
\end{code}

Sometimes we may choose to execute a PrimOp even though it isn't
certain that its result will be required; ie execute them
``speculatively''.  The same thing as ``cheap eagerness.'' Usually
this is OK, because PrimOps are usually cheap, but it isn't OK for
(a)~expensive PrimOps and (b)~PrimOps which can fail.

See also @primOpIsCheap@ (below).

PrimOps that have side effects also should not be executed speculatively
or by data dependencies.

\begin{code}
primOpOkForSpeculation :: PrimOp -> Bool
primOpOkForSpeculation op 
  = not (primOpCanFail op || primOpHasSideEffects op || primOpOutOfLine op)
\end{code}

@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once.  Evaluation order is unaffected.

\begin{code}
primOpIsCheap op = not (primOpHasSideEffects op || primOpOutOfLine op)
\end{code}

\begin{code}
primOpCanFail :: PrimOp -> Bool
-- Int.
primOpCanFail IntQuotOp	= True		-- Divide by zero
primOpCanFail IntRemOp		= True		-- Divide by zero

-- Integer
primOpCanFail IntegerQuotRemOp = True		-- Divide by zero
primOpCanFail IntegerDivModOp	= True		-- Divide by zero

-- Float.  ToDo: tan? tanh?
primOpCanFail FloatDivOp	= True		-- Divide by zero
primOpCanFail FloatLogOp	= True		-- Log of zero
primOpCanFail FloatAsinOp	= True		-- Arg out of domain
primOpCanFail FloatAcosOp	= True		-- Arg out of domain

-- Double.  ToDo: tan? tanh?
primOpCanFail DoubleDivOp	= True		-- Divide by zero
primOpCanFail DoubleLogOp	= True		-- Log of zero
primOpCanFail DoubleAsinOp	= True		-- Arg out of domain
primOpCanFail DoubleAcosOp	= True		-- Arg out of domain

primOpCanFail other_op		= False
\end{code}

And some primops have side-effects and so, for example, must not be
duplicated.

\begin{code}
primOpHasSideEffects :: PrimOp -> Bool

primOpHasSideEffects TakeMVarOp        = True
primOpHasSideEffects DelayOp           = True
primOpHasSideEffects WaitReadOp        = True
primOpHasSideEffects WaitWriteOp       = True

primOpHasSideEffects ParOp	       = True
primOpHasSideEffects ForkOp	       = True
primOpHasSideEffects KillThreadOp      = True
primOpHasSideEffects SeqOp	       = True

primOpHasSideEffects MakeForeignObjOp  = True
primOpHasSideEffects WriteForeignObjOp = True
primOpHasSideEffects MkWeakOp  	       = True
primOpHasSideEffects DeRefWeakOp       = True
primOpHasSideEffects MakeStablePtrOp   = True
primOpHasSideEffects EqStablePtrOp     = True  -- SOF
primOpHasSideEffects DeRefStablePtrOp  = True  -- ??? JSM & ADR

primOpHasSideEffects ParGlobalOp	= True
primOpHasSideEffects ParLocalOp		= True
primOpHasSideEffects ParAtOp		= True
primOpHasSideEffects ParAtAbsOp		= True
primOpHasSideEffects ParAtRelOp		= True
primOpHasSideEffects ParAtForNowOp	= True
primOpHasSideEffects CopyableOp		= True  -- Possibly not.  ASP 
primOpHasSideEffects NoFollowOp		= True  -- Possibly not.  ASP

-- CCall
primOpHasSideEffects (CCallOp	_ _ _ _) = True

primOpHasSideEffects other = False
\end{code}

Inline primitive operations that perform calls need wrappers to save
any live variables that are stored in caller-saves registers.

\begin{code}
primOpNeedsWrapper :: PrimOp -> Bool

primOpNeedsWrapper (CCallOp _ _ _ _)    = True

primOpNeedsWrapper Integer2IntOp    	= True
primOpNeedsWrapper Integer2WordOp    	= True
primOpNeedsWrapper IntegerCmpOp	    	= True

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
primOpNeedsWrapper FloatEncodeOp	= True

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
primOpNeedsWrapper DoubleEncodeOp    	= True

primOpNeedsWrapper MakeStablePtrOp	= True
primOpNeedsWrapper DeRefStablePtrOp	= True

primOpNeedsWrapper DelayOp	    	= True
primOpNeedsWrapper WaitReadOp		= True
primOpNeedsWrapper WaitWriteOp		= True

primOpNeedsWrapper other_op 	    	= False
\end{code}

\begin{code}
primOpOcc op
  = case (primOpInfo op) of
      Dyadic     occ _	       -> occ
      Monadic    occ _	       -> occ
      Compare    occ _	       -> occ
      GenPrimOp  occ _ _ _     -> occ
\end{code}

\begin{code}
primOpUniq :: PrimOp -> Unique
primOpUniq op = mkPrimOpIdUnique (IBOX(tagOf_PrimOp op))

primOpType :: PrimOp -> Type
primOpType op
  = case (primOpInfo op) of
      Dyadic occ ty ->	    dyadic_fun_ty ty
      Monadic occ ty ->	    monadic_fun_ty ty
      Compare occ ty ->	    compare_fun_ty ty

      GenPrimOp occ tyvars arg_tys res_ty -> 
	mkForAllTys tyvars (mkFunTys arg_tys res_ty)
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim	    PrimRep
  | ReturnsAlg	    TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo

getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty		 -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty		 -> ReturnsPrim (typePrimRep ty)
      Compare _ ty		 -> ReturnsAlg  boolTyCon
      GenPrimOp _ _ _ ty	 -> 
	let rep = typePrimRep ty in
	case rep of
	   PtrRep -> case splitAlgTyConApp_maybe ty of
			Nothing -> panic "getPrimOpResultInfo"
			Just (tc,_,_) -> ReturnsAlg tc
	   other -> ReturnsPrim other

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
commutableOp XorOp	  = True
commutableOp IntEqOp	  = True
commutableOp IntNeOp	  = True
commutableOp IntegerAddOp = True
commutableOp IntegerMulOp = True
commutableOp IntegerGcdOp = True
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
pprPrimOp  :: PrimOp -> SDoc

pprPrimOp (CCallOp fun is_casm may_gc cconv)
  = let
        callconv = text "{-" <> pprCallConv cconv <> text "-}"

	before
	  | is_casm && may_gc = "__casm_GC ``"
	  | is_casm	      = "__casm ``"
	  | may_gc	      = "__ccall_GC "
	  | otherwise	      = "__ccall "

	after
	  | is_casm   = text "''"
	  | otherwise = empty

	ppr_fun =
	 case fun of
	   Right _ -> ptext SLIT("<dynamic>")
	   Left fn -> ptext fn
	 
    in
    hcat [ ifPprDebug callconv
         , text before , ppr_fun , after]

pprPrimOp other_op
  = getPprStyle $ \ sty ->
   if ifaceStyle sty then	-- For interfaces Print it qualified with PrelGHC.
	ptext SLIT("PrelGHC.") <> pprOccName occ
   else
	pprOccName occ
  where
    occ = primOpOcc other_op
\end{code}
