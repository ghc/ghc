%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrimOp]{Primitive operations (machine-level)}

\begin{code}
module PrimOp (
	PrimOp(..), allThePrimOps,
	primOpType, primOpSig, primOpUsg, primOpArity,
	mkPrimOpIdName, primOpRdrName, primOpTag, primOpOcc,

	commutableOp,

	primOpOutOfLine, primOpNeedsWrapper, 
	primOpOkForSpeculation, primOpIsCheap, primOpIsDupable,
	primOpHasSideEffects,

	getPrimOpResultInfo,  PrimOpResultInfo(..),

	pprPrimOp,

	CCall(..), CCallTarget(..), ccallMayGC, ccallIsCasm, pprCCallOp
    ) where

#include "HsVersions.h"

import PrimRep		-- most of it
import TysPrim
import TysWiredIn

import Demand		( Demand, wwLazy, wwPrim, wwStrict, StrictnessInfo(..) )
import Var		( TyVar, Id )
import CallConv		( CallConv, pprCallConv )
import PprType		( pprParendType )
import Name		( Name, mkWiredInIdName )
import RdrName		( RdrName, mkRdrQual )
import OccName		( OccName, pprOccName, mkSrcVarOcc )
import TyCon		( TyCon, tyConArity )
import Type		( Type, mkForAllTys, mkForAllTy, mkFunTy, mkFunTys, mkTyVarTys,
			  mkTyConTy, mkTyConApp, typePrimRep,mkTyVarTy,
			  splitFunTy_maybe, splitAlgTyConApp_maybe, splitTyConApp_maybe,
                          UsageAnn(..), mkUsgTy
			)
import Unique		( Unique, mkPrimOpIdUnique )
import BasicTypes	( Arity )
import PrelMods		( pREL_GHC, pREL_GHC_Name )
import Outputable
import Util		( assoc, zipWithEqual )
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
    | IntAddOp | IntSubOp | IntMulOp | IntQuotOp
    | IntRemOp | IntNegOp
    | ISllOp | ISraOp | ISrlOp -- shift {left,right} {arithmetic,logical}
    | IntAddCOp
    | IntSubCOp
    | IntMulCOp
    | IntGcdOp

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
    | IntegerIntGcdOp | IntegerDivExactOp
    | IntegerQuotOp | IntegerRemOp

    | IntegerCmpOp
    | IntegerCmpIntOp

    | Integer2IntOp  | Integer2WordOp  
    | Int2IntegerOp  | Word2IntegerOp
    | Addr2IntegerOp
     -- casting to/from Integer and 64-bit (un)signed quantities.
    | IntegerToInt64Op | Int64ToIntegerOp
    | IntegerToWord64Op | Word64ToIntegerOp
    -- ?? gcd, etc?

    | FloatDecodeOp
    | DoubleDecodeOp

    -- primitive ops for primitive arrays

    | NewArrayOp
    | NewByteArrayOp PrimRep

    | SameMutableArrayOp
    | SameMutableByteArrayOp

    | ReadArrayOp | WriteArrayOp | IndexArrayOp -- for arrays of Haskell ptrs

    | ReadByteArrayOp	PrimRep
    | WriteByteArrayOp	PrimRep
    | IndexByteArrayOp	PrimRep
    | ReadOffAddrOp	PrimRep
    | WriteOffAddrOp    PrimRep
    | IndexOffAddrOp	PrimRep
	-- PrimRep can be one of :
	--	{Char,Int,Word,Addr,Float,Double,StablePtr,Int64,Word64}Rep.
	-- This is just a cheesy encoding of a bunch of ops.
	-- Note that ForeignObjRep is not included -- the only way of
	-- creating a ForeignObj is with a ccall or casm.
    | IndexOffForeignObjOp PrimRep

    | UnsafeFreezeArrayOp | UnsafeFreezeByteArrayOp
    | UnsafeThawArrayOp
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
    | IsEmptyMVarOp

    -- exceptions
    | CatchOp
    | RaiseOp
    | BlockAsyncExceptionsOp
    | UnblockAsyncExceptionsOp

    -- foreign objects
    | MakeForeignObjOp
    | WriteForeignObjOp

    -- weak pointers
    | MkWeakOp
    | DeRefWeakOp
    | FinalizeWeakOp

    -- stable names
    | MakeStableNameOp
    | EqStableNameOp
    | StableNameToIntOp

    -- stable pointers
    | MakeStablePtrOp
    | DeRefStablePtrOp
    | EqStablePtrOp

    -- Foreign calls
    | CCallOp CCall
    -- Operation to test two closure addresses for equality (yes really!)
    -- BLAME ALASTAIR REID FOR THIS!  THE REST OF US ARE INNOCENT!
    | ReallyUnsafePtrEqualityOp

    -- parallel stuff
    | SeqOp
    | ParOp

    -- concurrency
    | ForkOp
    | KillThreadOp
    | YieldOp
    | MyThreadIdOp
    | DelayOp
    | WaitReadOp
    | WaitWriteOp

    -- more parallel stuff
    | ParGlobalOp	-- named global par
    | ParLocalOp	-- named local par
    | ParAtOp		-- specifies destination of local par
    | ParAtAbsOp	-- specifies destination of local par (abs processor)
    | ParAtRelOp	-- specifies destination of local par (rel processor)
    | ParAtForNowOp	-- specifies initial destination of global par
    | CopyableOp	-- marks copyable code
    | NoFollowOp	-- marks non-followup expression

    -- tag-related
    | DataToTagOp
    | TagToEnumOp
\end{code}

Used for the Ord instance

\begin{code}
primOpTag :: PrimOp -> Int
primOpTag op = IBOX( tagOf_PrimOp op )

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
tagOf_PrimOp IntGcdOp			      = ILIT( 43)
tagOf_PrimOp IntRemOp			      = ILIT( 44)
tagOf_PrimOp IntNegOp			      = ILIT( 45)
tagOf_PrimOp WordQuotOp			      = ILIT( 47)
tagOf_PrimOp WordRemOp			      = ILIT( 48)
tagOf_PrimOp AndOp			      = ILIT( 49)
tagOf_PrimOp OrOp			      = ILIT( 50)
tagOf_PrimOp NotOp			      = ILIT( 51)
tagOf_PrimOp XorOp			      = ILIT( 52)
tagOf_PrimOp SllOp			      = ILIT( 53)
tagOf_PrimOp SrlOp			      = ILIT( 54)
tagOf_PrimOp ISllOp			      = ILIT( 55)
tagOf_PrimOp ISraOp			      = ILIT( 56)
tagOf_PrimOp ISrlOp			      = ILIT( 57)
tagOf_PrimOp IntAddCOp			      = ILIT( 58)
tagOf_PrimOp IntSubCOp			      = ILIT( 59)
tagOf_PrimOp IntMulCOp			      = ILIT( 60)
tagOf_PrimOp Int2WordOp			      = ILIT( 61)
tagOf_PrimOp Word2IntOp			      = ILIT( 62)
tagOf_PrimOp Int2AddrOp			      = ILIT( 63)
tagOf_PrimOp Addr2IntOp			      = ILIT( 64)
tagOf_PrimOp FloatAddOp			      = ILIT( 65)
tagOf_PrimOp FloatSubOp			      = ILIT( 66)
tagOf_PrimOp FloatMulOp			      = ILIT( 67)
tagOf_PrimOp FloatDivOp			      = ILIT( 68)
tagOf_PrimOp FloatNegOp			      = ILIT( 69)
tagOf_PrimOp Float2IntOp		      = ILIT( 70)
tagOf_PrimOp Int2FloatOp		      = ILIT( 71)
tagOf_PrimOp FloatExpOp			      = ILIT( 72)
tagOf_PrimOp FloatLogOp			      = ILIT( 73)
tagOf_PrimOp FloatSqrtOp		      = ILIT( 74)
tagOf_PrimOp FloatSinOp			      = ILIT( 75)
tagOf_PrimOp FloatCosOp			      = ILIT( 76)
tagOf_PrimOp FloatTanOp			      = ILIT( 77)
tagOf_PrimOp FloatAsinOp		      = ILIT( 78)
tagOf_PrimOp FloatAcosOp		      = ILIT( 79)
tagOf_PrimOp FloatAtanOp		      = ILIT( 80)
tagOf_PrimOp FloatSinhOp		      = ILIT( 81)
tagOf_PrimOp FloatCoshOp		      = ILIT( 82)
tagOf_PrimOp FloatTanhOp		      = ILIT( 83)
tagOf_PrimOp FloatPowerOp		      = ILIT( 84)
tagOf_PrimOp DoubleAddOp		      = ILIT( 85)
tagOf_PrimOp DoubleSubOp		      = ILIT( 86)
tagOf_PrimOp DoubleMulOp		      = ILIT( 87)
tagOf_PrimOp DoubleDivOp		      = ILIT( 88)
tagOf_PrimOp DoubleNegOp		      = ILIT( 89)
tagOf_PrimOp Double2IntOp		      = ILIT( 90)
tagOf_PrimOp Int2DoubleOp		      = ILIT( 91)
tagOf_PrimOp Double2FloatOp		      = ILIT( 92)
tagOf_PrimOp Float2DoubleOp		      = ILIT( 93)
tagOf_PrimOp DoubleExpOp		      = ILIT( 94)
tagOf_PrimOp DoubleLogOp		      = ILIT( 95)
tagOf_PrimOp DoubleSqrtOp		      = ILIT( 96)
tagOf_PrimOp DoubleSinOp		      = ILIT( 97)
tagOf_PrimOp DoubleCosOp		      = ILIT( 98)
tagOf_PrimOp DoubleTanOp		      = ILIT( 99)
tagOf_PrimOp DoubleAsinOp		      = ILIT(100)
tagOf_PrimOp DoubleAcosOp		      = ILIT(101)
tagOf_PrimOp DoubleAtanOp		      = ILIT(102)
tagOf_PrimOp DoubleSinhOp		      = ILIT(103)
tagOf_PrimOp DoubleCoshOp		      = ILIT(104)
tagOf_PrimOp DoubleTanhOp		      = ILIT(105)
tagOf_PrimOp DoublePowerOp		      = ILIT(106)
tagOf_PrimOp IntegerAddOp		      = ILIT(107)
tagOf_PrimOp IntegerSubOp		      = ILIT(108)
tagOf_PrimOp IntegerMulOp		      = ILIT(109)
tagOf_PrimOp IntegerGcdOp		      = ILIT(110)
tagOf_PrimOp IntegerIntGcdOp		      = ILIT(111)
tagOf_PrimOp IntegerDivExactOp		      = ILIT(112)
tagOf_PrimOp IntegerQuotOp		      = ILIT(113)
tagOf_PrimOp IntegerRemOp		      = ILIT(114)
tagOf_PrimOp IntegerQuotRemOp		      = ILIT(115)
tagOf_PrimOp IntegerDivModOp		      = ILIT(116)
tagOf_PrimOp IntegerNegOp		      = ILIT(117)
tagOf_PrimOp IntegerCmpOp		      = ILIT(118)
tagOf_PrimOp IntegerCmpIntOp		      = ILIT(119)
tagOf_PrimOp Integer2IntOp		      = ILIT(120)
tagOf_PrimOp Integer2WordOp		      = ILIT(121)
tagOf_PrimOp Int2IntegerOp		      = ILIT(122)
tagOf_PrimOp Word2IntegerOp		      = ILIT(123)
tagOf_PrimOp Addr2IntegerOp		      = ILIT(125)
tagOf_PrimOp IntegerToInt64Op		      = ILIT(127)
tagOf_PrimOp Int64ToIntegerOp		      = ILIT(128)
tagOf_PrimOp IntegerToWord64Op		      = ILIT(129)
tagOf_PrimOp Word64ToIntegerOp		      = ILIT(130)
tagOf_PrimOp FloatDecodeOp		      = ILIT(131)
tagOf_PrimOp DoubleDecodeOp		      = ILIT(132)
tagOf_PrimOp NewArrayOp			      = ILIT(133)
tagOf_PrimOp (NewByteArrayOp CharRep)	      = ILIT(134)
tagOf_PrimOp (NewByteArrayOp IntRep)	      = ILIT(135)
tagOf_PrimOp (NewByteArrayOp WordRep)	      = ILIT(136)
tagOf_PrimOp (NewByteArrayOp AddrRep)	      = ILIT(137)
tagOf_PrimOp (NewByteArrayOp FloatRep)	      = ILIT(138)
tagOf_PrimOp (NewByteArrayOp DoubleRep)       = ILIT(139)
tagOf_PrimOp (NewByteArrayOp StablePtrRep)    = ILIT(140)
tagOf_PrimOp SameMutableArrayOp		      = ILIT(141)
tagOf_PrimOp SameMutableByteArrayOp	      = ILIT(142)
tagOf_PrimOp ReadArrayOp		      = ILIT(143)
tagOf_PrimOp WriteArrayOp		      = ILIT(144)
tagOf_PrimOp IndexArrayOp		      = ILIT(145)
tagOf_PrimOp (ReadByteArrayOp CharRep)	      = ILIT(146)
tagOf_PrimOp (ReadByteArrayOp IntRep)	      = ILIT(147)
tagOf_PrimOp (ReadByteArrayOp WordRep)	      = ILIT(148)
tagOf_PrimOp (ReadByteArrayOp AddrRep)	      = ILIT(149)
tagOf_PrimOp (ReadByteArrayOp FloatRep)       = ILIT(150)
tagOf_PrimOp (ReadByteArrayOp DoubleRep)      = ILIT(151)
tagOf_PrimOp (ReadByteArrayOp StablePtrRep)   = ILIT(152)
tagOf_PrimOp (ReadByteArrayOp Int64Rep)	      = ILIT(153)
tagOf_PrimOp (ReadByteArrayOp Word64Rep)      = ILIT(154)
tagOf_PrimOp (WriteByteArrayOp CharRep)       = ILIT(155)
tagOf_PrimOp (WriteByteArrayOp IntRep)	      = ILIT(156)
tagOf_PrimOp (WriteByteArrayOp WordRep)	      = ILIT(157)
tagOf_PrimOp (WriteByteArrayOp AddrRep)       = ILIT(158)
tagOf_PrimOp (WriteByteArrayOp FloatRep)      = ILIT(159)
tagOf_PrimOp (WriteByteArrayOp DoubleRep)     = ILIT(160)
tagOf_PrimOp (WriteByteArrayOp StablePtrRep)  = ILIT(161)
tagOf_PrimOp (WriteByteArrayOp Int64Rep)      = ILIT(162)
tagOf_PrimOp (WriteByteArrayOp Word64Rep)     = ILIT(163)
tagOf_PrimOp (IndexByteArrayOp CharRep)       = ILIT(164)
tagOf_PrimOp (IndexByteArrayOp IntRep)	      = ILIT(165)
tagOf_PrimOp (IndexByteArrayOp WordRep)	      = ILIT(166)
tagOf_PrimOp (IndexByteArrayOp AddrRep)       = ILIT(167)
tagOf_PrimOp (IndexByteArrayOp FloatRep)      = ILIT(168)
tagOf_PrimOp (IndexByteArrayOp DoubleRep)     = ILIT(169)
tagOf_PrimOp (IndexByteArrayOp StablePtrRep)  = ILIT(170)
tagOf_PrimOp (IndexByteArrayOp Int64Rep)      = ILIT(171)
tagOf_PrimOp (IndexByteArrayOp Word64Rep)     = ILIT(172)
tagOf_PrimOp (IndexOffAddrOp CharRep)	      = ILIT(173)
tagOf_PrimOp (IndexOffAddrOp IntRep)	      = ILIT(174)
tagOf_PrimOp (IndexOffAddrOp WordRep)	      = ILIT(175)
tagOf_PrimOp (IndexOffAddrOp AddrRep)	      = ILIT(176)
tagOf_PrimOp (IndexOffAddrOp FloatRep)	      = ILIT(177)
tagOf_PrimOp (IndexOffAddrOp DoubleRep)       = ILIT(178)
tagOf_PrimOp (IndexOffAddrOp StablePtrRep)    = ILIT(179)
tagOf_PrimOp (IndexOffAddrOp Int64Rep)	      = ILIT(180)
tagOf_PrimOp (IndexOffAddrOp Word64Rep)	      = ILIT(181)
tagOf_PrimOp (IndexOffForeignObjOp CharRep)   = ILIT(182)
tagOf_PrimOp (IndexOffForeignObjOp IntRep)    = ILIT(183)
tagOf_PrimOp (IndexOffForeignObjOp WordRep)   = ILIT(184)
tagOf_PrimOp (IndexOffForeignObjOp AddrRep)   = ILIT(185)
tagOf_PrimOp (IndexOffForeignObjOp FloatRep)  = ILIT(186)
tagOf_PrimOp (IndexOffForeignObjOp DoubleRep) = ILIT(187)
tagOf_PrimOp (IndexOffForeignObjOp StablePtrRep) = ILIT(188)
tagOf_PrimOp (IndexOffForeignObjOp Int64Rep)  = ILIT(189)
tagOf_PrimOp (IndexOffForeignObjOp Word64Rep) = ILIT(190)
tagOf_PrimOp (ReadOffAddrOp CharRep)          = ILIT(191)
tagOf_PrimOp (ReadOffAddrOp IntRep)           = ILIT(192)
tagOf_PrimOp (ReadOffAddrOp WordRep)          = ILIT(193)
tagOf_PrimOp (ReadOffAddrOp AddrRep)          = ILIT(194)
tagOf_PrimOp (ReadOffAddrOp FloatRep)         = ILIT(195)
tagOf_PrimOp (ReadOffAddrOp DoubleRep)        = ILIT(196)
tagOf_PrimOp (ReadOffAddrOp StablePtrRep)     = ILIT(197)
tagOf_PrimOp (ReadOffAddrOp ForeignObjRep)    = ILIT(198)
tagOf_PrimOp (ReadOffAddrOp Int64Rep)         = ILIT(199)
tagOf_PrimOp (ReadOffAddrOp Word64Rep)        = ILIT(200)
tagOf_PrimOp (WriteOffAddrOp CharRep)         = ILIT(201)
tagOf_PrimOp (WriteOffAddrOp IntRep)          = ILIT(202)
tagOf_PrimOp (WriteOffAddrOp WordRep)         = ILIT(203)
tagOf_PrimOp (WriteOffAddrOp AddrRep)         = ILIT(205)
tagOf_PrimOp (WriteOffAddrOp FloatRep)        = ILIT(206)
tagOf_PrimOp (WriteOffAddrOp DoubleRep)       = ILIT(207)
tagOf_PrimOp (WriteOffAddrOp StablePtrRep)    = ILIT(208)
tagOf_PrimOp (WriteOffAddrOp ForeignObjRep)   = ILIT(209)
tagOf_PrimOp (WriteOffAddrOp Int64Rep)        = ILIT(210)
tagOf_PrimOp (WriteOffAddrOp Word64Rep)       = ILIT(211)
tagOf_PrimOp UnsafeFreezeArrayOp	      = ILIT(212)
tagOf_PrimOp UnsafeFreezeByteArrayOp	      = ILIT(213)
tagOf_PrimOp UnsafeThawArrayOp		      = ILIT(214)
tagOf_PrimOp SizeofByteArrayOp		      = ILIT(215)
tagOf_PrimOp SizeofMutableByteArrayOp	      = ILIT(216)
tagOf_PrimOp NewMVarOp			      = ILIT(217)
tagOf_PrimOp TakeMVarOp		    	      = ILIT(218)
tagOf_PrimOp PutMVarOp		    	      = ILIT(219)
tagOf_PrimOp SameMVarOp		    	      = ILIT(220)
tagOf_PrimOp IsEmptyMVarOp	    	      = ILIT(221)
tagOf_PrimOp MakeForeignObjOp		      = ILIT(222)
tagOf_PrimOp WriteForeignObjOp		      = ILIT(223)
tagOf_PrimOp MkWeakOp			      = ILIT(224)
tagOf_PrimOp DeRefWeakOp		      = ILIT(225)
tagOf_PrimOp FinalizeWeakOp		      = ILIT(226)
tagOf_PrimOp MakeStableNameOp		      = ILIT(227)
tagOf_PrimOp EqStableNameOp		      = ILIT(228)
tagOf_PrimOp StableNameToIntOp		      = ILIT(229)
tagOf_PrimOp MakeStablePtrOp		      = ILIT(230)
tagOf_PrimOp DeRefStablePtrOp		      = ILIT(231)
tagOf_PrimOp EqStablePtrOp		      = ILIT(232)
tagOf_PrimOp ReallyUnsafePtrEqualityOp	      = ILIT(234)
tagOf_PrimOp SeqOp			      = ILIT(235)
tagOf_PrimOp ParOp			      = ILIT(236)
tagOf_PrimOp ForkOp			      = ILIT(237)
tagOf_PrimOp KillThreadOp		      = ILIT(238)
tagOf_PrimOp YieldOp			      = ILIT(239)
tagOf_PrimOp MyThreadIdOp		      = ILIT(240)
tagOf_PrimOp DelayOp			      = ILIT(241)
tagOf_PrimOp WaitReadOp			      = ILIT(242)
tagOf_PrimOp WaitWriteOp		      = ILIT(243)
tagOf_PrimOp ParGlobalOp		      = ILIT(244)
tagOf_PrimOp ParLocalOp			      = ILIT(245)
tagOf_PrimOp ParAtOp			      = ILIT(246)
tagOf_PrimOp ParAtAbsOp			      = ILIT(247)
tagOf_PrimOp ParAtRelOp			      = ILIT(248)
tagOf_PrimOp ParAtForNowOp		      = ILIT(249)
tagOf_PrimOp CopyableOp			      = ILIT(250)
tagOf_PrimOp NoFollowOp			      = ILIT(251)
tagOf_PrimOp NewMutVarOp		      = ILIT(252)
tagOf_PrimOp ReadMutVarOp		      = ILIT(253)
tagOf_PrimOp WriteMutVarOp		      = ILIT(254)
tagOf_PrimOp SameMutVarOp		      = ILIT(255)
tagOf_PrimOp CatchOp			      = ILIT(256)
tagOf_PrimOp RaiseOp			      = ILIT(257)
tagOf_PrimOp BlockAsyncExceptionsOp	      = ILIT(258)
tagOf_PrimOp UnblockAsyncExceptionsOp	      = ILIT(259)
tagOf_PrimOp DataToTagOp		      = ILIT(260)
tagOf_PrimOp TagToEnumOp		      = ILIT(261)

tagOf_PrimOp op = pprPanic# "tagOf_PrimOp: pattern-match" (ppr op)

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
allThePrimOps		-- Except CCall, which is really a family of primops
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
	IntGcdOp,
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
	IntAddCOp,
	IntSubCOp,
	IntMulCOp,
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
        IntegerIntGcdOp,
        IntegerDivExactOp,
        IntegerQuotOp,
        IntegerRemOp,
	IntegerQuotRemOp,
	IntegerDivModOp,
	IntegerNegOp,
	IntegerCmpOp,
	IntegerCmpIntOp,
	Integer2IntOp,
	Integer2WordOp,
	Int2IntegerOp,
	Word2IntegerOp,
	Addr2IntegerOp,
	IntegerToInt64Op,
	Int64ToIntegerOp,
	IntegerToWord64Op,
	Word64ToIntegerOp,
	FloatDecodeOp,
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
	ReadOffAddrOp CharRep,
	ReadOffAddrOp IntRep,
	ReadOffAddrOp WordRep,
	ReadOffAddrOp AddrRep,
	ReadOffAddrOp FloatRep,
	ReadOffAddrOp DoubleRep,
	ReadOffAddrOp ForeignObjRep,
	ReadOffAddrOp StablePtrRep,
	ReadOffAddrOp Int64Rep,
	ReadOffAddrOp Word64Rep,
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
	UnsafeThawArrayOp,
	SizeofByteArrayOp,
	SizeofMutableByteArrayOp,
	NewMutVarOp,
	ReadMutVarOp,
	WriteMutVarOp,
	SameMutVarOp,
        CatchOp,
        RaiseOp,
	BlockAsyncExceptionsOp,
	UnblockAsyncExceptionsOp,
    	NewMVarOp,
	TakeMVarOp,
	PutMVarOp,
	SameMVarOp,
	IsEmptyMVarOp,
	MakeForeignObjOp,
	WriteForeignObjOp,
	MkWeakOp,
	DeRefWeakOp,
	FinalizeWeakOp,
	MakeStableNameOp,
	EqStableNameOp,
	StableNameToIntOp,
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
	YieldOp,
	MyThreadIdOp,
	DelayOp,
	WaitReadOp,
	WaitWriteOp,
	DataToTagOp,
	TagToEnumOp
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

mkDyadic str  ty = Dyadic  (mkSrcVarOcc str) ty
mkMonadic str ty = Monadic (mkSrcVarOcc str) ty
mkCompare str ty = Compare (mkSrcVarOcc str) ty
mkGenPrimOp str tvs tys ty = GenPrimOp (mkSrcVarOcc str) tvs tys ty
\end{code}

Utility bits:
\begin{code}
one_Integer_ty = [intPrimTy, byteArrayPrimTy]
two_Integer_tys
  = [intPrimTy, byteArrayPrimTy, -- first Integer pieces
     intPrimTy, byteArrayPrimTy] -- second '' pieces
an_Integer_and_Int_tys
  = [intPrimTy, byteArrayPrimTy, -- Integer
     intPrimTy]

unboxedPair	 = mkUnboxedTupleTy 2
unboxedTriple    = mkUnboxedTupleTy 3
unboxedQuadruple = mkUnboxedTupleTy 4

mkIOTy ty = mkFunTy realWorldStatePrimTy 
		    (unboxedPair [realWorldStatePrimTy,ty])

integerMonadic name = mkGenPrimOp name [] one_Integer_ty 
			(unboxedPair one_Integer_ty)

integerDyadic name = mkGenPrimOp name [] two_Integer_tys 
			(unboxedPair one_Integer_ty)

integerDyadic2Results name = mkGenPrimOp name [] two_Integer_tys 
    (unboxedQuadruple two_Integer_tys)

integerCompare name = mkGenPrimOp name [] two_Integer_tys intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection{Strictness}
%*									*
%************************************************************************

Not all primops are strict!

\begin{code}
primOpStrictness :: Arity -> PrimOp -> StrictnessInfo
	-- See Demand.StrictnessInfo for discussion of what the results
	-- The arity should be the arity of the primop; that's why
	-- this function isn't exported.

primOpStrictness arity SeqOp            = StrictnessInfo [wwStrict] False
	-- Seq is strict in its argument; see notes in ConFold.lhs

primOpStrictness arity ParOp            = StrictnessInfo [wwLazy] False
	-- Note that Par is lazy to avoid that the sparked thing
	-- gets evaluted strictly, which it should *not* be

primOpStrictness arity ForkOp		= StrictnessInfo [wwLazy, wwPrim] False

primOpStrictness arity NewArrayOp       = StrictnessInfo [wwPrim, wwLazy, wwPrim] False
primOpStrictness arity WriteArrayOp     = StrictnessInfo [wwPrim, wwPrim, wwLazy, wwPrim] False

primOpStrictness arity NewMutVarOp	= StrictnessInfo [wwLazy, wwPrim] False
primOpStrictness arity WriteMutVarOp	= StrictnessInfo [wwPrim, wwLazy, wwPrim] False

primOpStrictness arity PutMVarOp	= StrictnessInfo [wwPrim, wwLazy, wwPrim] False

primOpStrictness arity CatchOp	 		= StrictnessInfo [wwLazy, wwLazy, wwPrim] False
	-- Catch is actually strict in its first argument
	-- but we don't want to tell the strictness
	-- analyser about that!

primOpStrictness arity RaiseOp	  		= StrictnessInfo [wwLazy] True	-- NB: True => result is bottom
primOpStrictness arity BlockAsyncExceptionsOp   = StrictnessInfo [wwLazy] False
primOpStrictness arity UnblockAsyncExceptionsOp = StrictnessInfo [wwLazy] False

primOpStrictness arity MkWeakOp		= StrictnessInfo [wwLazy, wwLazy, wwLazy, wwPrim] False
primOpStrictness arity MakeStableNameOp = StrictnessInfo [wwLazy, wwPrim] False
primOpStrictness arity MakeStablePtrOp  = StrictnessInfo [wwLazy, wwPrim] False

primOpStrictness arity DataToTagOp      = StrictnessInfo [wwLazy] False

	-- The rest all have primitive-typed arguments
primOpStrictness arity other		= StrictnessInfo (replicate arity wwPrim) False
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
primOpInfo IntAddOp  = mkDyadic SLIT("+#")	    intPrimTy
primOpInfo IntSubOp  = mkDyadic SLIT("-#") 	    intPrimTy
primOpInfo IntMulOp  = mkDyadic SLIT("*#") 	    intPrimTy
primOpInfo IntQuotOp = mkDyadic SLIT("quotInt#")    intPrimTy
primOpInfo IntRemOp  = mkDyadic SLIT("remInt#")	    intPrimTy
primOpInfo IntGcdOp  = mkDyadic SLIT("gcdInt#")	    intPrimTy

primOpInfo IntNegOp  = mkMonadic SLIT("negateInt#") intPrimTy

primOpInfo IntAddCOp = 
	mkGenPrimOp SLIT("addIntC#")  [] [intPrimTy, intPrimTy] 
		(unboxedPair [intPrimTy, intPrimTy])

primOpInfo IntSubCOp = 
	mkGenPrimOp SLIT("subIntC#")  [] [intPrimTy, intPrimTy] 
		(unboxedPair [intPrimTy, intPrimTy])

primOpInfo IntMulCOp = 
	mkGenPrimOp SLIT("mulIntC#")  [] [intPrimTy, intPrimTy] 
		(unboxedPair [intPrimTy, intPrimTy])
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

@decodeFloat#@ is given w/ Integer-stuff (it's similar).

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

@decodeDouble#@ is given w/ Integer-stuff (it's similar).

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
primOpInfo IntegerIntGcdOp = mkGenPrimOp SLIT("gcdIntegerInt#") [] an_Integer_and_Int_tys intPrimTy
primOpInfo IntegerDivExactOp  = integerDyadic SLIT("divExactInteger#")
primOpInfo IntegerQuotOp = integerDyadic SLIT("quotInteger#")
primOpInfo IntegerRemOp  = integerDyadic SLIT("remInteger#")

primOpInfo IntegerCmpOp	= integerCompare SLIT("cmpInteger#")
primOpInfo IntegerCmpIntOp 
  = mkGenPrimOp SLIT("cmpIntegerInt#") [] an_Integer_and_Int_tys intPrimTy

primOpInfo IntegerQuotRemOp = integerDyadic2Results SLIT("quotRemInteger#")
primOpInfo IntegerDivModOp  = integerDyadic2Results SLIT("divModInteger#")

primOpInfo Integer2IntOp
  = mkGenPrimOp SLIT("integer2Int#") [] one_Integer_ty intPrimTy

primOpInfo Integer2WordOp
  = mkGenPrimOp SLIT("integer2Word#") [] one_Integer_ty wordPrimTy

primOpInfo Int2IntegerOp
  = mkGenPrimOp SLIT("int2Integer#") [] [intPrimTy] 
	(unboxedPair one_Integer_ty)

primOpInfo Word2IntegerOp
  = mkGenPrimOp SLIT("word2Integer#") [] [wordPrimTy] 
	(unboxedPair one_Integer_ty)

primOpInfo Addr2IntegerOp
  = mkGenPrimOp SLIT("addr2Integer#") [] [addrPrimTy] 
	(unboxedPair one_Integer_ty)

primOpInfo IntegerToInt64Op
  = mkGenPrimOp SLIT("integerToInt64#") [] one_Integer_ty int64PrimTy

primOpInfo Int64ToIntegerOp
  = mkGenPrimOp SLIT("int64ToInteger#") [] [int64PrimTy]
	(unboxedPair one_Integer_ty)

primOpInfo Word64ToIntegerOp
  = mkGenPrimOp SLIT("word64ToInteger#") [] [word64PrimTy] 
	(unboxedPair one_Integer_ty)

primOpInfo IntegerToWord64Op
  = mkGenPrimOp SLIT("integerToWord64#") [] one_Integer_ty word64PrimTy
\end{code}

Decoding of floating-point numbers is sorta Integer-related.  Encoding
is done with plain ccalls now (see PrelNumExtra.lhs).

\begin{code}
primOpInfo FloatDecodeOp
  = mkGenPrimOp SLIT("decodeFloat#") [] [floatPrimTy] 
	(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])
primOpInfo DoubleDecodeOp
  = mkGenPrimOp SLIT("decodeDouble#") [] [doublePrimTy] 
	(unboxedTriple [intPrimTy, intPrimTy, byteArrayPrimTy])
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-Arrays]{PrimOpInfo for primitive arrays}
%*									*
%************************************************************************

\begin{verbatim}
newArray#    :: Int# -> a -> State# s -> (# State# s, MutArr# s a #)
newFooArray# :: Int# -> State# s -> (# State# s, MutByteArr# s #)
\end{verbatim}

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

{-
sameMutableArray#     :: MutArr# s a -> MutArr# s a -> Bool
sameMutableByteArray# :: MutByteArr# s -> MutByteArr# s -> Bool
-}

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

{-
readArray#  :: MutArr# s a -> Int# -> State# s -> (# State# s, a #)
writeArray# :: MutArr# s a -> Int# -> a -> State# s -> State# s
indexArray# :: Array# a -> Int# -> (# a #)
-}

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
	(mkUnboxedTupleTy 1 [elt])

---------------------------------------------------------------------------
-- Primitive arrays full of unboxed bytes:

primOpInfo (ReadByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar

	op_str	       = _PK_ ("read" ++ primRepString kind ++ "Array#")
	(tvs, prim_ty) = mkPrimTyApp betaTyVars kind
	state          = mkStatePrimTy s
    in
    mkGenPrimOp op_str (s_tv:tvs)
	[mkMutableByteArrayPrimTy s, intPrimTy, state]
	(unboxedPair [state, prim_ty])

primOpInfo (WriteByteArrayOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar
	op_str = _PK_ ("write" ++ primRepString kind ++ "Array#")
	(tvs, prim_ty) = mkPrimTyApp betaTyVars kind
    in
    mkGenPrimOp op_str (s_tv:tvs)
	[mkMutableByteArrayPrimTy s, intPrimTy, prim_ty, mkStatePrimTy s]
	(mkStatePrimTy s)

primOpInfo (IndexByteArrayOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "Array#")
        (tvs, prim_ty) = mkPrimTyApp alphaTyVars kind
    in
    mkGenPrimOp op_str tvs [byteArrayPrimTy, intPrimTy] prim_ty

primOpInfo (IndexOffForeignObjOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "OffForeignObj#")
        (tvs, prim_ty) = mkPrimTyApp alphaTyVars kind
    in
    mkGenPrimOp op_str tvs [foreignObjPrimTy, intPrimTy] prim_ty

primOpInfo (IndexOffAddrOp kind)
  = let
	op_str = _PK_ ("index" ++ primRepString kind ++ "OffAddr#")
        (tvs, prim_ty) = mkPrimTyApp alphaTyVars kind
    in
    mkGenPrimOp op_str tvs [addrPrimTy, intPrimTy] prim_ty

primOpInfo (ReadOffAddrOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar
	op_str = _PK_ ("read" ++ primRepString kind ++ "OffAddr#")
        (tvs, prim_ty) = mkPrimTyApp betaTyVars kind
	state          = mkStatePrimTy s
    in
    mkGenPrimOp op_str (s_tv:tvs)
	[addrPrimTy, intPrimTy, state]
	(unboxedPair [state, prim_ty])

primOpInfo (WriteOffAddrOp kind)
  = let
	s = alphaTy; s_tv = alphaTyVar
	op_str = _PK_ ("write" ++ primRepString kind ++ "OffAddr#")
        (tvs, prim_ty) = mkPrimTyApp betaTyVars kind
    in
    mkGenPrimOp op_str (s_tv:tvs)
	[addrPrimTy, intPrimTy, prim_ty, mkStatePrimTy s]
	(mkStatePrimTy s)

---------------------------------------------------------------------------
{-
unsafeFreezeArray#     :: MutArr# s a -> State# s -> (# State# s, Array# a #)
unsafeFreezeByteArray# :: MutByteArr# s -> State# s -> (# State# s, ByteArray# #)
unsafeThawArray#       :: Array# a -> State# s -> (# State# s, MutArr# s a #)
-}

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

primOpInfo UnsafeThawArrayOp
  = let {
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar;
	state = mkStatePrimTy s
    } in
    mkGenPrimOp SLIT("unsafeThawArray#") [s_tv, elt_tv]
	[mkArrayPrimTy elt, state]
	(unboxedPair [state, mkMutableArrayPrimTy s elt])

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

catch# :: (State# RealWorld -> (# State# RealWorld, a))
       -> (b -> State# RealWorld -> (# State# RealWorld, a)) 
       -> State# RealWorld
       -> (# State# RealWorld, a)

throw  :: Exception -> a
raise# :: a -> b

blockAsyncExceptions#   :: IO a -> IO a
unblockAsyncExceptions# :: IO a -> IO a

\begin{code}
primOpInfo CatchOp   
  = let
	a = alphaTy; a_tv = alphaTyVar
	b = betaTy;  b_tv = betaTyVar;
	io_a = mkIOTy a
    in
    mkGenPrimOp SLIT("catch#") [a_tv, b_tv] 
	  [io_a, mkFunTy b io_a, realWorldStatePrimTy]
	  (unboxedPair [realWorldStatePrimTy, a])

primOpInfo RaiseOp
  = let
	a = alphaTy; a_tv = alphaTyVar
	b = betaTy;  b_tv = betaTyVar;
    in
    mkGenPrimOp SLIT("raise#") [a_tv, b_tv] [a] b

primOpInfo BlockAsyncExceptionsOp
  = let
      a = alphaTy; a_tv = alphaTyVar
    in
    mkGenPrimOp SLIT("blockAsyncExceptions#") [a_tv]
	[ mkIOTy a, realWorldStatePrimTy ]
	(unboxedPair [realWorldStatePrimTy,a])
	
primOpInfo UnblockAsyncExceptionsOp
  = let
      a = alphaTy; a_tv = alphaTyVar
    in
    mkGenPrimOp SLIT("unblockAsyncExceptions#") [a_tv]
	[ mkIOTy a, realWorldStatePrimTy ]
	(unboxedPair [realWorldStatePrimTy,a])
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

primOpInfo IsEmptyMVarOp
  = let
	elt = alphaTy; elt_tv = alphaTyVar; s = betaTy; s_tv = betaTyVar
	state = mkStatePrimTy s
    in
    mkGenPrimOp SLIT("isEmptyMVar#") [s_tv, elt_tv]
	[mkMVarPrimTy s elt, mkStatePrimTy s]
	(unboxedPair [state, intPrimTy])

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

-- killThread# :: ThreadId# -> exception -> State# RealWorld -> State# RealWorld
primOpInfo KillThreadOp
  = mkGenPrimOp SLIT("killThread#") [alphaTyVar] 
	[threadIdPrimTy, alphaTy, realWorldStatePrimTy]
	realWorldStatePrimTy

-- yield# :: State# RealWorld -> State# RealWorld
primOpInfo YieldOp
  = mkGenPrimOp SLIT("yield#") [] 
	[realWorldStatePrimTy]
	realWorldStatePrimTy

-- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
primOpInfo MyThreadIdOp
  = mkGenPrimOp SLIT("myThreadId#") [] 
	[realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, threadIdPrimTy])
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
  = mkGenPrimOp SLIT("mkWeak#") [openAlphaTyVar, betaTyVar, gammaTyVar] 
	[mkTyVarTy openAlphaTyVar, betaTy, gammaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, mkWeakPrimTy betaTy])
\end{code}

The following operation dereferences a weak pointer.  The weak pointer
may have been finalized, so the operation returns a result code which
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

Weak pointers can be finalized early by using the finalize# operation:
	
	finalizeWeak# :: Weak# v -> State# RealWorld -> 
	   		   (# State# RealWorld, Int#, IO () #)

The Int# returned is either

	0 if the weak pointer has already been finalized, or it has no
	  finalizer (the third component is then invalid).

	1 if the weak pointer is still alive, with the finalizer returned
	  as the third component.

\begin{code}
primOpInfo FinalizeWeakOp
 = mkGenPrimOp SLIT("finalizeWeak#") [alphaTyVar]
	[mkWeakPrimTy alphaTy, realWorldStatePrimTy]
	(unboxedTriple [realWorldStatePrimTy, intPrimTy,
		        mkFunTy realWorldStatePrimTy 
			  (unboxedPair [realWorldStatePrimTy,unitTy])])
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-stable-pointers]{PrimOpInfo for stable pointers and stable names}
%*									*
%************************************************************************

A {\em stable name/pointer} is an index into a table of stable name
entries.  Since the garbage collector is told about stable pointers,
it is safe to pass a stable pointer to external systems such as C
routines.

\begin{verbatim}
makeStablePtr#  :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
freeStablePtr   :: StablePtr# a -> State# RealWorld -> State# RealWorld
deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
eqStablePtr#    :: StablePtr# a -> StablePtr# a -> Int#
\end{verbatim}

It may seem a bit surprising that @makeStablePtr#@ is a @IO@
operation since it doesn't (directly) involve IO operations.  The
reason is that if some optimisation pass decided to duplicate calls to
@makeStablePtr#@ and we only pass one of the stable pointers over, a
massive space leak can result.  Putting it into the IO monad
prevents this.  (Another reason for putting them in a monad is to
ensure correct sequencing wrt the side-effecting @freeStablePtr@
operation.)

An important property of stable pointers is that if you call
makeStablePtr# twice on the same object you get the same stable
pointer back.

Note that we can implement @freeStablePtr#@ using @_ccall_@ (and,
besides, it's not likely to be used from Haskell) so it's not a
primop.

Question: Why @RealWorld@ - won't any instance of @_ST@ do the job? [ADR]

Stable Names
~~~~~~~~~~~~

A stable name is like a stable pointer, but with three important differences:

	(a) You can't deRef one to get back to the original object.
	(b) You can convert one to an Int.
	(c) You don't need to 'freeStableName'

The existence of a stable name doesn't guarantee to keep the object it
points to alive (unlike a stable pointer), hence (a).

Invariants:
	
	(a) makeStableName always returns the same value for a given
	    object (same as stable pointers).

	(b) if two stable names are equal, it implies that the objects
	    from which they were created were the same.

	(c) stableNameToInt always returns the same Int for a given
	    stable name.

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

primOpInfo MakeStableNameOp
  = mkGenPrimOp SLIT("makeStableName#") [alphaTyVar]
	[alphaTy, realWorldStatePrimTy]
	(unboxedPair [realWorldStatePrimTy, 
			mkTyConApp stableNamePrimTyCon [alphaTy]])

primOpInfo EqStableNameOp
  = mkGenPrimOp SLIT("eqStableName#") [alphaTyVar, betaTyVar]
	[mkStableNamePrimTy alphaTy, mkStableNamePrimTy betaTy]
	intPrimTy

primOpInfo StableNameToIntOp
  = mkGenPrimOp SLIT("stableNameToInt#") [alphaTyVar]
	[mkStableNamePrimTy alphaTy]
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
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

primOpInfo ParGlobalOp	-- parGlobal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parGlobal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParLocalOp	-- parLocal# :: a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parLocal#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtOp	-- parAt# :: a -> v -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parAt#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTy

primOpInfo ParAtAbsOp	-- parAtAbs# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parAtAbs#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtRelOp	-- parAtRel# :: a -> Int# -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parAtRel#")	[alphaTyVar,betaTyVar] [alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,betaTy] intPrimTy

primOpInfo ParAtForNowOp -- parAtForNow# :: a -> v -> Int# -> Int# -> Int# -> Int# -> b -> Int#
  = mkGenPrimOp SLIT("parAtForNow#")	[alphaTyVar,betaTyVar,gammaTyVar] [betaTy,alphaTy,intPrimTy,intPrimTy,intPrimTy,intPrimTy,gammaTy] intPrimTy

primOpInfo CopyableOp	-- copyable# :: a -> Int#
  = mkGenPrimOp SLIT("copyable#")	[alphaTyVar] [alphaTy] intPrimTy

primOpInfo NoFollowOp	-- noFollow# :: a -> Int#
  = mkGenPrimOp SLIT("noFollow#")	[alphaTyVar] [alphaTy] intPrimTy
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-tag]{PrimOpInfo for @dataToTag#@ and @tagToEnum#@}
%*									*
%************************************************************************

These primops are pretty wierd.

	dataToTag# :: a -> Int    (arg must be an evaluated data type)
	tagToEnum# :: Int -> a    (result type must be an enumerated type)

The constraints aren't currently checked by the front end, but the
code generator will fall over if they aren't satisfied.

\begin{code}
primOpInfo DataToTagOp
  = mkGenPrimOp SLIT("dataToTag#") [alphaTyVar] [alphaTy] intPrimTy

primOpInfo TagToEnumOp
  = mkGenPrimOp SLIT("tagToEnum#") [alphaTyVar] [intPrimTy] alphaTy

#ifdef DEBUG
primOpInfo op = pprPanic "primOpInfo:" (ppr op)
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-ool]{Which PrimOps are out-of-line}
%*									*
%************************************************************************

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.

\begin{code}
primOpOutOfLine op
  = case op of
    	TakeMVarOp    		     -> True
	PutMVarOp     		     -> True
	DelayOp       		     -> True
	WaitReadOp    		     -> True
	WaitWriteOp   		     -> True
	CatchOp	      		     -> True
	RaiseOp	      		     -> True
	BlockAsyncExceptionsOp       -> True
	UnblockAsyncExceptionsOp     -> True
	NewArrayOp    		     -> True
	NewByteArrayOp _ 	     -> True
	IntegerAddOp    	     -> True
	IntegerSubOp    	     -> True
	IntegerMulOp    	     -> True
	IntegerGcdOp    	     -> True
	IntegerDivExactOp    	     -> True
	IntegerQuotOp    	     -> True
	IntegerRemOp    	     -> True
	IntegerQuotRemOp    	     -> True
	IntegerDivModOp    	     -> True
	Int2IntegerOp		     -> True
	Word2IntegerOp  	     -> True
	Addr2IntegerOp		     -> True
	Word64ToIntegerOp            -> True
	Int64ToIntegerOp             -> True
	FloatDecodeOp		     -> True
	DoubleDecodeOp		     -> True
	MkWeakOp		     -> True
	FinalizeWeakOp		     -> True
	MakeStableNameOp	     -> True
	MakeForeignObjOp	     -> True
	NewMutVarOp		     -> True
	NewMVarOp		     -> True
	ForkOp			     -> True
	KillThreadOp		     -> True
	YieldOp			     -> True

	UnsafeThawArrayOp            -> True
	  -- UnsafeThawArrayOp doesn't perform any heap checks,
	  -- but it is of such an esoteric nature that
	  -- it is done out-of-line rather than require
	  -- the NCG to implement it.

	CCallOp ccall -> ccallMayGC ccall

	other -> False
\end{code}


primOpOkForSpeculation
~~~~~~~~~~~~~~~~~~~~~~
Sometimes we may choose to execute a PrimOp even though it isn't
certain that its result will be required; ie execute them
``speculatively''.  The same thing as ``cheap eagerness.'' Usually
this is OK, because PrimOps are usually cheap, but it isn't OK for
(a)~expensive PrimOps and (b)~PrimOps which can fail.

PrimOps that have side effects also should not be executed speculatively.

Ok-for-speculation also means that it's ok *not* to execute the
primop. For example
	case op a b of
	  r -> 3
Here the result is not used, so we can discard the primop.  Anything
that has side effects mustn't be dicarded in this way, of course!

See also @primOpIsCheap@ (below).


\begin{code}
primOpOkForSpeculation :: PrimOp -> Bool
	-- See comments with CoreUtils.exprOkForSpeculation
primOpOkForSpeculation op 
  = not (primOpCanFail op || primOpHasSideEffects op || primOpOutOfLine op)
\end{code}


primOpIsCheap
~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once.  Evaluation order is unaffected.

\begin{code}
primOpIsCheap :: PrimOp -> Bool
	-- See comments with CoreUtils.exprOkForSpeculation
primOpIsCheap op = not (primOpHasSideEffects op || primOpOutOfLine op)
\end{code}

primOpIsDupable
~~~~~~~~~~~~~~~
primOpIsDupable means that the use of the primop is small enough to
duplicate into different case branches.  See CoreUtils.exprIsDupable.

\begin{code}
primOpIsDupable :: PrimOp -> Bool
	-- See comments with CoreUtils.exprIsDupable
	-- We say it's dupable it isn't implemented by a C call with a wrapper
primOpIsDupable op = not (primOpNeedsWrapper op)
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

primOpHasSideEffects ParOp	       = True
primOpHasSideEffects ForkOp	       = True
primOpHasSideEffects KillThreadOp      = True
primOpHasSideEffects YieldOp	       = True
primOpHasSideEffects SeqOp	       = True

primOpHasSideEffects MakeForeignObjOp  = True
primOpHasSideEffects WriteForeignObjOp = True
primOpHasSideEffects MkWeakOp  	       = True
primOpHasSideEffects DeRefWeakOp       = True
primOpHasSideEffects FinalizeWeakOp    = True
primOpHasSideEffects MakeStablePtrOp   = True
primOpHasSideEffects MakeStableNameOp  = True
primOpHasSideEffects EqStablePtrOp     = True  -- SOF
primOpHasSideEffects DeRefStablePtrOp  = True  -- ??? JSM & ADR

-- In general, writes are considered a side effect, but 
--	reads and variable allocations are not
-- Why?  Because writes must not be omitted, but reads can be if their result is not used.
-- (Sequencing of reads is maintained by data dependencies on the resulting
-- world state.)
primOpHasSideEffects WriteArrayOp	   = True
primOpHasSideEffects (WriteByteArrayOp _)  = True
primOpHasSideEffects (WriteOffAddrOp _)	   = True
primOpHasSideEffects WriteMutVarOp	   = True

primOpHasSideEffects UnsafeFreezeArrayOp	= True
primOpHasSideEffects UnsafeFreezeByteArrayOp	= True
primOpHasSideEffects UnsafeThawArrayOp		= True

primOpHasSideEffects TakeMVarOp        = True
primOpHasSideEffects PutMVarOp         = True
primOpHasSideEffects DelayOp           = True
primOpHasSideEffects WaitReadOp        = True
primOpHasSideEffects WaitWriteOp       = True

primOpHasSideEffects ParGlobalOp	= True
primOpHasSideEffects ParLocalOp		= True
primOpHasSideEffects ParAtOp		= True
primOpHasSideEffects ParAtAbsOp		= True
primOpHasSideEffects ParAtRelOp		= True
primOpHasSideEffects ParAtForNowOp	= True
primOpHasSideEffects CopyableOp		= True  -- Possibly not.  ASP 
primOpHasSideEffects NoFollowOp		= True  -- Possibly not.  ASP
primOpHasSideEffects (CCallOp _) 	= True

primOpHasSideEffects other = False
\end{code}

Inline primitive operations that perform calls need wrappers to save
any live variables that are stored in caller-saves registers.

\begin{code}
primOpNeedsWrapper :: PrimOp -> Bool

primOpNeedsWrapper (CCallOp _) 		= True

primOpNeedsWrapper Integer2IntOp    	= True
primOpNeedsWrapper Integer2WordOp    	= True
primOpNeedsWrapper IntegerCmpOp	    	= True
primOpNeedsWrapper IntegerCmpIntOp    	= True

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

primOpNeedsWrapper MakeStableNameOp	= True
primOpNeedsWrapper DeRefStablePtrOp	= True

primOpNeedsWrapper DelayOp	    	= True
primOpNeedsWrapper WaitReadOp		= True
primOpNeedsWrapper WaitWriteOp		= True

primOpNeedsWrapper other_op 	    	= False
\end{code}

\begin{code}
primOpArity :: PrimOp -> Arity
primOpArity op 
  = case (primOpInfo op) of
      Monadic occ ty			  -> 1
      Dyadic occ ty			  -> 2
      Compare occ ty 			  -> 2
      GenPrimOp occ tyvars arg_tys res_ty -> length arg_tys
		
primOpType :: PrimOp -> Type  -- you may want to use primOpSig instead
primOpType op
  = case (primOpInfo op) of
      Dyadic occ ty ->	    dyadic_fun_ty ty
      Monadic occ ty ->	    monadic_fun_ty ty
      Compare occ ty ->	    compare_fun_ty ty

      GenPrimOp occ tyvars arg_tys res_ty -> 
	mkForAllTys tyvars (mkFunTys arg_tys res_ty)

mkPrimOpIdName :: PrimOp -> Id -> Name
	-- Make the name for the PrimOp's Id
	-- We have to pass in the Id itself because it's a WiredInId
	-- and hence recursive
mkPrimOpIdName op id
  = mkWiredInIdName key pREL_GHC occ_name id
  where
    occ_name = primOpOcc op
    key	     = mkPrimOpIdUnique (primOpTag op)


primOpRdrName :: PrimOp -> RdrName 
primOpRdrName op = mkRdrQual pREL_GHC_Name (primOpOcc op)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case (primOpInfo op) of
			      Dyadic    occ _	  -> occ
			      Monadic   occ _	  -> occ
			      Compare   occ _	  -> occ
			      GenPrimOp occ _ _ _ -> occ

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOp -> ([TyVar], [Type], Type, Arity, StrictnessInfo)
primOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness arity op)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo op) of
	  Monadic   occ ty -> ([],     [ty],    ty    )
	  Dyadic    occ ty -> ([],     [ty,ty], ty    )
	  Compare   occ ty -> ([],     [ty,ty], boolTy)
	  GenPrimOp occ tyvars arg_tys res_ty
                           -> (tyvars, arg_tys, res_ty)

-- primOpUsg is like primOpSig but the types it yields are the
-- appropriate sigma (i.e., usage-annotated) types,
-- as required by the UsageSP inference.

primOpUsg :: PrimOp -> ([TyVar],[Type],Type)
primOpUsg op
  = case op of

      -- Refer to comment by `otherwise' clause; we need consider here
      -- *only* primops that have arguments or results containing Haskell
      -- pointers (things that are pointed).  Unpointed values are
      -- irrelevant to the usage analysis.  The issue is whether pointed
      -- values may be entered or duplicated by the primop.

      -- Remember that primops are *never* partially applied.

      NewArrayOp           -> mangle [mkP, mkM, mkP     ] mkM
      SameMutableArrayOp   -> mangle [mkP, mkP          ] mkM
      ReadArrayOp          -> mangle [mkM, mkP, mkP     ] mkM
      WriteArrayOp         -> mangle [mkM, mkP, mkM, mkP] mkR
      IndexArrayOp         -> mangle [mkM, mkP          ] mkM
      UnsafeFreezeArrayOp  -> mangle [mkM, mkP          ] mkM
      UnsafeThawArrayOp    -> mangle [mkM, mkP          ] mkM

      NewMutVarOp          -> mangle [mkM, mkP          ] mkM
      ReadMutVarOp         -> mangle [mkM, mkP          ] mkM
      WriteMutVarOp        -> mangle [mkM, mkM, mkP     ] mkR
      SameMutVarOp         -> mangle [mkP, mkP          ] mkM

      CatchOp              -> --     [mkO, mkO . (inFun mkM mkO)] mkO
                              mangle [mkM, mkM . (inFun mkM mkM), mkP] mkM
                              -- might use caught action multiply
      RaiseOp              -> mangle [mkM               ] mkM

      NewMVarOp            -> mangle [mkP               ] mkR
      TakeMVarOp           -> mangle [mkM, mkP          ] mkM
      PutMVarOp            -> mangle [mkM, mkM, mkP     ] mkR
      SameMVarOp           -> mangle [mkP, mkP          ] mkM
      IsEmptyMVarOp        -> mangle [mkP, mkP          ] mkM

      ForkOp               -> mangle [mkO, mkP          ] mkR
      KillThreadOp         -> mangle [mkP, mkM, mkP     ] mkR

      MkWeakOp             -> mangle [mkZ, mkM, mkM, mkP] mkM
      DeRefWeakOp          -> mangle [mkM, mkP          ] mkM
      FinalizeWeakOp       -> mangle [mkM, mkP          ] (mkR . (inUB [id,id,inFun mkR mkM]))

      MakeStablePtrOp      -> mangle [mkM, mkP          ] mkM
      DeRefStablePtrOp     -> mangle [mkM, mkP          ] mkM
      EqStablePtrOp        -> mangle [mkP, mkP          ] mkR
      MakeStableNameOp     -> mangle [mkZ, mkP          ] mkR
      EqStableNameOp       -> mangle [mkP, mkP          ] mkR
      StableNameToIntOp    -> mangle [mkP               ] mkR

      ReallyUnsafePtrEqualityOp -> mangle [mkZ, mkZ     ] mkR

      SeqOp                -> mangle [mkO               ] mkR
      ParOp                -> mangle [mkO               ] mkR
      ParGlobalOp          -> mangle [mkO, mkP, mkP, mkP, mkP, mkM] mkM
      ParLocalOp           -> mangle [mkO, mkP, mkP, mkP, mkP, mkM] mkM
      ParAtOp              -> mangle [mkO, mkZ, mkP, mkP, mkP, mkP, mkM] mkM
      ParAtAbsOp           -> mangle [mkO, mkP, mkP, mkP, mkP, mkM] mkM
      ParAtRelOp           -> mangle [mkO, mkP, mkP, mkP, mkP, mkM] mkM
      ParAtForNowOp        -> mangle [mkO, mkZ, mkP, mkP, mkP, mkP, mkM] mkM
      CopyableOp           -> mangle [mkZ               ] mkR
      NoFollowOp           -> mangle [mkZ               ] mkR

      CCallOp _ 	   -> mangle [                  ] mkM

      -- Things with no Haskell pointers inside: in actuality, usages are
      -- irrelevant here (hence it doesn't matter that some of these
      -- apparently permit duplication; since such arguments are never 
      -- ENTERed anyway, the usage annotation they get is entirely irrelevant
      -- except insofar as it propagates to infect other values that *are*
      -- pointed.

      otherwise            -> nomangle
                                    
  where mkZ          = mkUsgTy UsOnce  -- pointed argument used zero
        mkO          = mkUsgTy UsOnce  -- pointed argument used once
        mkM          = mkUsgTy UsMany  -- pointed argument used multiply
        mkP          = mkUsgTy UsOnce  -- unpointed argument
        mkR          = mkUsgTy UsMany  -- unpointed result
  
        (tyvars, arg_tys, res_ty, _, _) = primOpSig op

        nomangle     = (tyvars, map mkP arg_tys, mkR res_ty)

        mangle fs g  = (tyvars, zipWithEqual "primOpUsg" ($) fs arg_tys, g res_ty)

        inFun f g ty = case splitFunTy_maybe ty of
                         Just (a,b) -> mkFunTy (f a) (g b)
                         Nothing    -> pprPanic "primOpUsg:inFun" (ppr op <+> ppr ty)

        inUB fs ty  = case splitTyConApp_maybe ty of
                        Just (tc,tys) -> ASSERT( tc == unboxedTupleTyCon (length fs) )
                                         mkUnboxedTupleTy (length fs) (zipWithEqual "primOpUsg"
                                                                         ($) fs tys)
                        Nothing       -> pprPanic "primOpUsg:inUB" (ppr op <+> ppr ty)
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim	    PrimRep
  | ReturnsAlg	    TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo (CCallOp _)
  = ReturnsAlg unboxedPairTyCon
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty		 -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty		 -> ReturnsPrim (typePrimRep ty)
      Compare _ ty		 -> ReturnsAlg boolTyCon
      GenPrimOp _ _ _ ty	 -> 
	let rep = typePrimRep ty in
	case rep of
	   PtrRep -> case splitAlgTyConApp_maybe ty of
			Nothing -> panic "getPrimOpResultInfo"
			Just (tc,_,_) -> ReturnsAlg tc
	   other -> ReturnsPrim other
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
commutableOp IntegerIntGcdOp = True
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
mkPrimTyApp :: [TyVar] -> PrimRep -> ([TyVar], Type)
	-- CharRep       -->  ([],  Char#)
	-- StablePtrRep  -->  ([a], StablePtr# a)
mkPrimTyApp tvs kind
  = (forall_tvs, mkTyConApp tycon (mkTyVarTys forall_tvs))
  where
    tycon      = primRepTyCon kind
    forall_tvs = take (tyConArity tycon) tvs

dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] boolTy
\end{code}

Output stuff:
\begin{code}
pprPrimOp  :: PrimOp -> SDoc

pprPrimOp (CCallOp ccall) = pprCCallOp ccall
pprPrimOp other_op
  = getPprStyle $ \ sty ->
    if ifaceStyle sty then	-- For interfaces Print it qualified with PrelGHC.
	ptext SLIT("PrelGHC.") <> pprOccName occ
    else
	pprOccName occ
  where
    occ = primOpOcc other_op
\end{code}


\end{code}


%************************************************************************
%*									*
\subsubsection{CCalls}
%*									*
%************************************************************************

A special ``trap-door'' to use in making calls direct to C functions:
\begin{code}
data CCall
  =  CCall	CCallTarget
		Bool		-- True <=> really a "casm"
		Bool		-- True <=> might invoke Haskell GC
		CallConv	-- calling convention to use.

data CCallTarget
  = StaticTarget  FAST_STRING   -- An "unboxed" ccall# to `fn'.
  | DynamicTarget Unique	-- First argument (an Addr#) is the function pointer
				--   (unique is used to generate a 'typedef' to cast
				--    the function pointer if compiling the ccall# down to
				--    .hc code - can't do this inline for tedious reasons.)

ccallMayGC :: CCall -> Bool
ccallMayGC (CCall _ _ may_gc _) = may_gc

ccallIsCasm :: CCall -> Bool
ccallIsCasm (CCall _ c_asm _ _) = c_asm
\end{code}

\begin{code}
pprCCallOp (CCall fun is_casm may_gc cconv)
  = hcat [ ifPprDebug callconv
	 , text "__", ppr_dyn
         , text before , ppr_fun , after]
  where
        callconv = text "{-" <> pprCallConv cconv <> text "-}"

	before
	  | is_casm && may_gc = "casm_GC ``"
	  | is_casm	      = "casm ``"
	  | may_gc	      = "ccall_GC "
	  | otherwise	      = "ccall "

	after
	  | is_casm   = text "''"
	  | otherwise = empty
	  
	ppr_dyn = case fun of
		    DynamicTarget _ -> text "dyn_"
		    _	   	    -> empty

	ppr_fun = case fun of
		     DynamicTarget _ -> text "\"\""
		     StaticTarget fn -> ptext fn
\end{code}
