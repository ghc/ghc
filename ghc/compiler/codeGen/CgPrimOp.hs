-----------------------------------------------------------------------------
--
-- Code generation for PrimOps.
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------

module CgPrimOp (
   cgPrimOp
 ) where

#include "HsVersions.h"

import ForeignCall	( CCallConv(CCallConv) )
import StgSyn		( StgLiveVars, StgArg )
import CgBindery	( getVolatileRegs, getArgAmodes )
import CgMonad
import CgInfoTbls	( getConstrTag )
import CgUtils		( cmmOffsetW, cmmOffsetB, cmmLoadIndexW )
import ForeignCall
import Cmm
import CLabel		( mkMAP_FROZEN_infoLabel, mkMAP_DIRTY_infoLabel,
			  mkDirty_MUT_VAR_Label, mkRtsCodeLabel )
import CmmUtils
import MachOp
import SMRep
import PrimOp		( PrimOp(..) )
import SMRep		( tablesNextToCode )
import Constants 	( wORD_SIZE, wORD_SIZE_IN_BITS )
import StaticFlags	( opt_Parallel )
import Outputable

-- ---------------------------------------------------------------------------
-- Code generation for PrimOps

cgPrimOp   :: [CmmReg] 		-- where to put the results
	   -> PrimOp		-- the op
	   -> [StgArg]		-- arguments
	   -> StgLiveVars	-- live vars, in case we need to save them
	   -> Code

cgPrimOp results op args live
  = do arg_exprs <- getArgAmodes args
       let non_void_args = [ e | (r,e) <- arg_exprs, nonVoidArg r ] 
       emitPrimOp results op non_void_args live


emitPrimOp :: [CmmReg] 		-- where to put the results
	   -> PrimOp		-- the op
	   -> [CmmExpr]		-- arguments
	   -> StgLiveVars	-- live vars, in case we need to save them
	   -> Code

--  First we handle various awkward cases specially.  The remaining
-- easy cases are then handled by translateOp, defined below.

emitPrimOp [res_r,res_c] IntAddCOp [aa,bb] live
{- 
   With some bit-twiddling, we can define int{Add,Sub}Czh portably in
   C, and without needing any comparisons.  This may not be the
   fastest way to do it - if you have better code, please send it! --SDM
  
   Return : r = a + b,  c = 0 if no overflow, 1 on overflow.
  
   We currently don't make use of the r value if c is != 0 (i.e. 
   overflow), we just convert to big integers and try again.  This
   could be improved by making r and c the correct values for
   plugging into a new J#.  
   
   { r = ((I_)(a)) + ((I_)(b));					\
     c = ((StgWord)(~(((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
         >> (BITS_IN (I_) - 1);					\
   } 
   Wading through the mass of bracketry, it seems to reduce to:
   c = ( (~(a^b)) & (a^r) ) >>unsigned (BITS_IN(I_)-1)

-}
   = stmtsC [
        CmmAssign res_r (CmmMachOp mo_wordAdd [aa,bb]),
        CmmAssign res_c $
	  CmmMachOp mo_wordUShr [
		CmmMachOp mo_wordAnd [
		    CmmMachOp mo_wordNot [CmmMachOp mo_wordXor [aa,bb]],
		    CmmMachOp mo_wordXor [aa, CmmReg res_r]
		], 
	        CmmLit (mkIntCLit (wORD_SIZE_IN_BITS - 1))
	  ]
     ]


emitPrimOp [res_r,res_c] IntSubCOp [aa,bb] live
{- Similarly:
   #define subIntCzh(r,c,a,b)					\
   { r = ((I_)(a)) - ((I_)(b));					\
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
         >> (BITS_IN (I_) - 1);					\
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)
-}
   = stmtsC [
        CmmAssign res_r (CmmMachOp mo_wordSub [aa,bb]),
        CmmAssign res_c $
	  CmmMachOp mo_wordUShr [
		CmmMachOp mo_wordAnd [
		    CmmMachOp mo_wordXor [aa,bb],
		    CmmMachOp mo_wordXor [aa, CmmReg res_r]
		], 
	        CmmLit (mkIntCLit (wORD_SIZE_IN_BITS - 1))
	  ]
     ]


emitPrimOp [res] ParOp [arg] live
  = do
	-- for now, just implement this in a C function
	-- later, we might want to inline it.
    vols <- getVolatileRegs live
    stmtC (CmmCall (CmmForeignCall newspark CCallConv) [(res,NoHint)]
		[(CmmReg (CmmGlobal BaseReg), PtrHint), 
		 (arg,PtrHint)] 
		(Just vols))
  where
	newspark = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("newSpark")))

emitPrimOp [res] ReadMutVarOp [mutv] live
   = stmtC (CmmAssign res (cmmLoadIndexW mutv fixedHdrSize))

emitPrimOp [] WriteMutVarOp [mutv,var] live
   = do
	stmtC (CmmStore (cmmOffsetW mutv fixedHdrSize) var)
	vols <- getVolatileRegs live
	stmtC (CmmCall (CmmForeignCall (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
				CCallConv) 
			[{-no results-}]
			[(mutv,PtrHint)]
			(Just vols))

--  #define sizzeofByteArrayzh(r,a) \
--     r = (((StgArrWords *)(a))->words * sizeof(W_))
emitPrimOp [res] SizeofByteArrayOp [arg] live
   = stmtC $
	CmmAssign res (CmmMachOp mo_wordMul [
			  cmmLoadIndexW arg fixedHdrSize,
			  CmmLit (mkIntCLit wORD_SIZE)
			])

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = (((StgArrWords *)(a))->words * sizeof(W_))
emitPrimOp [res] SizeofMutableByteArrayOp [arg] live
   = emitPrimOp [res] SizeofByteArrayOp [arg] live


--  #define touchzh(o)                  /* nothing */
emitPrimOp [] TouchOp [arg] live
   = nopC

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
emitPrimOp [res] ByteArrayContents_Char [arg] live
   = stmtC (CmmAssign res (cmmOffsetB arg arrWordsHdrSize))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
emitPrimOp [res] StableNameToIntOp [arg] live
   = stmtC (CmmAssign res (cmmLoadIndexW arg fixedHdrSize))

--  #define eqStableNamezh(r,sn1,sn2)					\
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
emitPrimOp [res] EqStableNameOp [arg1,arg2] live
   = stmtC (CmmAssign res (CmmMachOp mo_wordEq [
				cmmLoadIndexW arg1 fixedHdrSize,
				cmmLoadIndexW arg2 fixedHdrSize
			 ]))


emitPrimOp [res] ReallyUnsafePtrEqualityOp [arg1,arg2] live
   = stmtC (CmmAssign res (CmmMachOp mo_wordEq [arg1,arg2]))

--  #define addrToHValuezh(r,a) r=(P_)a
emitPrimOp [res] AddrToHValueOp [arg] live
   = stmtC (CmmAssign res arg)

--  #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
emitPrimOp [res] DataToTagOp [arg] live
   = stmtC (CmmAssign res (getConstrTag arg))

{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 -}

--  #define unsafeFreezzeArrayzh(r,a)
--	{
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN0_info);
--	  r = a;
--	}
emitPrimOp [res] UnsafeFreezeArrayOp [arg] live
   = stmtsC [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
	     CmmAssign res arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)	r=(a)
emitPrimOp [res] UnsafeFreezeByteArrayOp [arg] live
   = stmtC (CmmAssign res arg)

-- Reading/writing pointer arrays

emitPrimOp [r] ReadArrayOp  [obj,ix]   live  = doReadPtrArrayOp r obj ix
emitPrimOp [r] IndexArrayOp [obj,ix]   live  = doReadPtrArrayOp r obj ix
emitPrimOp []  WriteArrayOp [obj,ix,v] live  = doWritePtrArrayOp obj ix v

-- IndexXXXoffAddr

emitPrimOp res IndexOffAddrOp_Char      args live = doIndexOffAddrOp (Just mo_u_8ToWord) I8 res args
emitPrimOp res IndexOffAddrOp_WideChar  args live = doIndexOffAddrOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res IndexOffAddrOp_Int       args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res IndexOffAddrOp_Word      args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res IndexOffAddrOp_Addr      args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res IndexOffAddrOp_Float     args live = doIndexOffAddrOp Nothing F32 res args
emitPrimOp res IndexOffAddrOp_Double    args live = doIndexOffAddrOp Nothing F64 res args
emitPrimOp res IndexOffAddrOp_StablePtr args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res IndexOffAddrOp_Int8      args live = doIndexOffAddrOp (Just mo_s_8ToWord) I8  res args
emitPrimOp res IndexOffAddrOp_Int16     args live = doIndexOffAddrOp (Just mo_s_16ToWord) I16 res args
emitPrimOp res IndexOffAddrOp_Int32     args live = doIndexOffAddrOp (Just mo_s_32ToWord) I32 res args
emitPrimOp res IndexOffAddrOp_Int64     args live = doIndexOffAddrOp Nothing I64 res args
emitPrimOp res IndexOffAddrOp_Word8     args live = doIndexOffAddrOp (Just mo_u_8ToWord) I8  res args
emitPrimOp res IndexOffAddrOp_Word16    args live = doIndexOffAddrOp (Just mo_u_16ToWord) I16 res args
emitPrimOp res IndexOffAddrOp_Word32    args live = doIndexOffAddrOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res IndexOffAddrOp_Word64    args live = doIndexOffAddrOp Nothing I64 res args

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

emitPrimOp res ReadOffAddrOp_Char      args live = doIndexOffAddrOp (Just mo_u_8ToWord) I8 res args
emitPrimOp res ReadOffAddrOp_WideChar  args live = doIndexOffAddrOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res ReadOffAddrOp_Int       args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res ReadOffAddrOp_Word      args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res ReadOffAddrOp_Addr      args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res ReadOffAddrOp_Float     args live = doIndexOffAddrOp Nothing F32 res args
emitPrimOp res ReadOffAddrOp_Double    args live = doIndexOffAddrOp Nothing F64 res args
emitPrimOp res ReadOffAddrOp_StablePtr args live = doIndexOffAddrOp Nothing wordRep res args
emitPrimOp res ReadOffAddrOp_Int8      args live = doIndexOffAddrOp (Just mo_s_8ToWord) I8  res args
emitPrimOp res ReadOffAddrOp_Int16     args live = doIndexOffAddrOp (Just mo_s_16ToWord) I16 res args
emitPrimOp res ReadOffAddrOp_Int32     args live = doIndexOffAddrOp (Just mo_s_32ToWord) I32 res args
emitPrimOp res ReadOffAddrOp_Int64     args live = doIndexOffAddrOp Nothing I64 res args
emitPrimOp res ReadOffAddrOp_Word8     args live = doIndexOffAddrOp (Just mo_u_8ToWord) I8  res args
emitPrimOp res ReadOffAddrOp_Word16    args live = doIndexOffAddrOp (Just mo_u_16ToWord) I16 res args
emitPrimOp res ReadOffAddrOp_Word32    args live = doIndexOffAddrOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res ReadOffAddrOp_Word64    args live = doIndexOffAddrOp Nothing I64 res args

-- IndexXXXArray

emitPrimOp res IndexByteArrayOp_Char      args live = doIndexByteArrayOp (Just mo_u_8ToWord) I8 res args
emitPrimOp res IndexByteArrayOp_WideChar  args live = doIndexByteArrayOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res IndexByteArrayOp_Int       args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res IndexByteArrayOp_Word      args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res IndexByteArrayOp_Addr      args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res IndexByteArrayOp_Float     args live = doIndexByteArrayOp Nothing F32 res args
emitPrimOp res IndexByteArrayOp_Double    args live = doIndexByteArrayOp Nothing F64 res args
emitPrimOp res IndexByteArrayOp_StablePtr args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res IndexByteArrayOp_Int8      args live = doIndexByteArrayOp (Just mo_s_8ToWord) I8  res args
emitPrimOp res IndexByteArrayOp_Int16     args live = doIndexByteArrayOp (Just mo_s_16ToWord) I16  res args
emitPrimOp res IndexByteArrayOp_Int32     args live = doIndexByteArrayOp (Just mo_s_32ToWord) I32  res args
emitPrimOp res IndexByteArrayOp_Int64     args live = doIndexByteArrayOp Nothing I64  res args
emitPrimOp res IndexByteArrayOp_Word8     args live = doIndexByteArrayOp (Just mo_u_8ToWord) I8  res args
emitPrimOp res IndexByteArrayOp_Word16    args live = doIndexByteArrayOp (Just mo_u_16ToWord) I16  res args
emitPrimOp res IndexByteArrayOp_Word32    args live = doIndexByteArrayOp (Just mo_u_32ToWord) I32  res args
emitPrimOp res IndexByteArrayOp_Word64    args live = doIndexByteArrayOp Nothing I64  res args

-- ReadXXXArray, identical to IndexXXXArray.

emitPrimOp res ReadByteArrayOp_Char       args live = doIndexByteArrayOp (Just mo_u_8ToWord) I8 res args
emitPrimOp res ReadByteArrayOp_WideChar   args live = doIndexByteArrayOp (Just mo_u_32ToWord) I32 res args
emitPrimOp res ReadByteArrayOp_Int        args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res ReadByteArrayOp_Word       args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res ReadByteArrayOp_Addr       args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res ReadByteArrayOp_Float      args live = doIndexByteArrayOp Nothing F32 res args
emitPrimOp res ReadByteArrayOp_Double     args live = doIndexByteArrayOp Nothing F64 res args
emitPrimOp res ReadByteArrayOp_StablePtr  args live = doIndexByteArrayOp Nothing wordRep res args
emitPrimOp res ReadByteArrayOp_Int8       args live = doIndexByteArrayOp (Just mo_s_8ToWord) I8  res args
emitPrimOp res ReadByteArrayOp_Int16      args live = doIndexByteArrayOp (Just mo_s_16ToWord) I16  res args
emitPrimOp res ReadByteArrayOp_Int32      args live = doIndexByteArrayOp (Just mo_s_32ToWord) I32  res args
emitPrimOp res ReadByteArrayOp_Int64      args live = doIndexByteArrayOp Nothing I64  res args
emitPrimOp res ReadByteArrayOp_Word8      args live = doIndexByteArrayOp (Just mo_u_8ToWord) I8  res args
emitPrimOp res ReadByteArrayOp_Word16     args live = doIndexByteArrayOp (Just mo_u_16ToWord) I16  res args
emitPrimOp res ReadByteArrayOp_Word32     args live = doIndexByteArrayOp (Just mo_u_32ToWord) I32  res args
emitPrimOp res ReadByteArrayOp_Word64     args live = doIndexByteArrayOp Nothing I64  res args

-- WriteXXXoffAddr

emitPrimOp res WriteOffAddrOp_Char       args live = doWriteOffAddrOp (Just mo_WordTo8) I8 res args
emitPrimOp res WriteOffAddrOp_WideChar   args live = doWriteOffAddrOp (Just mo_WordTo32) I32 res args
emitPrimOp res WriteOffAddrOp_Int        args live = doWriteOffAddrOp Nothing wordRep res args
emitPrimOp res WriteOffAddrOp_Word       args live = doWriteOffAddrOp Nothing wordRep res args
emitPrimOp res WriteOffAddrOp_Addr       args live = doWriteOffAddrOp Nothing wordRep res args
emitPrimOp res WriteOffAddrOp_Float      args live = doWriteOffAddrOp Nothing F32 res args
emitPrimOp res WriteOffAddrOp_Double     args live = doWriteOffAddrOp Nothing F64 res args
emitPrimOp res WriteOffAddrOp_StablePtr  args live = doWriteOffAddrOp Nothing wordRep res args
emitPrimOp res WriteOffAddrOp_Int8       args live = doWriteOffAddrOp (Just mo_WordTo8) I8  res args
emitPrimOp res WriteOffAddrOp_Int16      args live = doWriteOffAddrOp (Just mo_WordTo16) I16 res args
emitPrimOp res WriteOffAddrOp_Int32      args live = doWriteOffAddrOp (Just mo_WordTo32) I32 res args
emitPrimOp res WriteOffAddrOp_Int64      args live = doWriteOffAddrOp Nothing I64 res args
emitPrimOp res WriteOffAddrOp_Word8      args live = doWriteOffAddrOp (Just mo_WordTo8) I8  res args
emitPrimOp res WriteOffAddrOp_Word16     args live = doWriteOffAddrOp (Just mo_WordTo16) I16 res args
emitPrimOp res WriteOffAddrOp_Word32     args live = doWriteOffAddrOp (Just mo_WordTo32) I32 res args
emitPrimOp res WriteOffAddrOp_Word64     args live = doWriteOffAddrOp Nothing I64 res args

-- WriteXXXArray

emitPrimOp res WriteByteArrayOp_Char      args live = doWriteByteArrayOp (Just mo_WordTo8) I8 res args
emitPrimOp res WriteByteArrayOp_WideChar  args live = doWriteByteArrayOp (Just mo_WordTo32) I32 res args
emitPrimOp res WriteByteArrayOp_Int       args live = doWriteByteArrayOp Nothing wordRep res args
emitPrimOp res WriteByteArrayOp_Word      args live = doWriteByteArrayOp Nothing wordRep res args
emitPrimOp res WriteByteArrayOp_Addr      args live = doWriteByteArrayOp Nothing wordRep res args
emitPrimOp res WriteByteArrayOp_Float     args live = doWriteByteArrayOp Nothing F32 res args
emitPrimOp res WriteByteArrayOp_Double    args live = doWriteByteArrayOp Nothing F64 res args
emitPrimOp res WriteByteArrayOp_StablePtr args live = doWriteByteArrayOp Nothing wordRep res args
emitPrimOp res WriteByteArrayOp_Int8      args live = doWriteByteArrayOp (Just mo_WordTo8) I8  res args
emitPrimOp res WriteByteArrayOp_Int16     args live = doWriteByteArrayOp (Just mo_WordTo16) I16  res args
emitPrimOp res WriteByteArrayOp_Int32     args live = doWriteByteArrayOp (Just mo_WordTo32) I32  res args
emitPrimOp res WriteByteArrayOp_Int64     args live = doWriteByteArrayOp Nothing I64  res args
emitPrimOp res WriteByteArrayOp_Word8     args live = doWriteByteArrayOp (Just mo_WordTo8) I8  res args
emitPrimOp res WriteByteArrayOp_Word16    args live = doWriteByteArrayOp (Just mo_WordTo16) I16  res args
emitPrimOp res WriteByteArrayOp_Word32    args live = doWriteByteArrayOp (Just mo_WordTo32) I32  res args
emitPrimOp res WriteByteArrayOp_Word64    args live = doWriteByteArrayOp Nothing I64  res args


-- The rest just translate straightforwardly
emitPrimOp [res] op [arg] live
   | nopOp op
   = stmtC (CmmAssign res arg)

   | Just (mop,rep) <- narrowOp op
   = stmtC (CmmAssign res (CmmMachOp (mop rep wordRep) [
			  CmmMachOp (mop wordRep rep) [arg]]))

emitPrimOp [res] op args live
   | Just prim <- callishOp op
   = do vols <- getVolatileRegs live
	stmtC (CmmCall (CmmPrim prim) [(res,NoHint)] 
		[(a,NoHint) | a<-args] (Just vols)) -- ToDo: hints?

   | Just mop <- translateOp op
   = let stmt = CmmAssign res (CmmMachOp mop args) in
     stmtC stmt

emitPrimOp _ op _ _
 = pprPanic "emitPrimOp: can't translate PrimOp" (ppr op)


-- These PrimOps are NOPs in Cmm

nopOp Int2WordOp     = True
nopOp Word2IntOp     = True
nopOp Int2AddrOp     = True
nopOp Addr2IntOp     = True
nopOp ChrOp	     = True  -- Int# and Char# are rep'd the same
nopOp OrdOp	     = True
nopOp _		     = False

-- These PrimOps turn into double casts

narrowOp Narrow8IntOp   = Just (MO_S_Conv, I8)
narrowOp Narrow16IntOp  = Just (MO_S_Conv, I16)
narrowOp Narrow32IntOp  = Just (MO_S_Conv, I32)
narrowOp Narrow8WordOp  = Just (MO_U_Conv, I8)
narrowOp Narrow16WordOp = Just (MO_U_Conv, I16)
narrowOp Narrow32WordOp = Just (MO_U_Conv, I32)
narrowOp _ 		= Nothing

-- Native word signless ops

translateOp IntAddOp       = Just mo_wordAdd
translateOp IntSubOp       = Just mo_wordSub
translateOp WordAddOp      = Just mo_wordAdd
translateOp WordSubOp      = Just mo_wordSub
translateOp AddrAddOp      = Just mo_wordAdd
translateOp AddrSubOp      = Just mo_wordSub

translateOp IntEqOp        = Just mo_wordEq
translateOp IntNeOp        = Just mo_wordNe
translateOp WordEqOp       = Just mo_wordEq
translateOp WordNeOp       = Just mo_wordNe
translateOp AddrEqOp       = Just mo_wordEq
translateOp AddrNeOp       = Just mo_wordNe

translateOp AndOp          = Just mo_wordAnd
translateOp OrOp           = Just mo_wordOr
translateOp XorOp          = Just mo_wordXor
translateOp NotOp          = Just mo_wordNot
translateOp SllOp	   = Just mo_wordShl
translateOp SrlOp	   = Just mo_wordUShr

translateOp AddrRemOp	   = Just mo_wordURem

-- Native word signed ops

translateOp IntMulOp        = Just mo_wordMul
translateOp IntMulMayOfloOp = Just (MO_S_MulMayOflo wordRep)
translateOp IntQuotOp       = Just mo_wordSQuot
translateOp IntRemOp        = Just mo_wordSRem
translateOp IntNegOp        = Just mo_wordSNeg


translateOp IntGeOp        = Just mo_wordSGe
translateOp IntLeOp        = Just mo_wordSLe
translateOp IntGtOp        = Just mo_wordSGt
translateOp IntLtOp        = Just mo_wordSLt

translateOp ISllOp	   = Just mo_wordShl
translateOp ISraOp	   = Just mo_wordSShr
translateOp ISrlOp	   = Just mo_wordUShr

-- Native word unsigned ops

translateOp WordGeOp       = Just mo_wordUGe
translateOp WordLeOp       = Just mo_wordULe
translateOp WordGtOp       = Just mo_wordUGt
translateOp WordLtOp       = Just mo_wordULt

translateOp WordMulOp      = Just mo_wordMul
translateOp WordQuotOp     = Just mo_wordUQuot
translateOp WordRemOp      = Just mo_wordURem

translateOp AddrGeOp       = Just mo_wordUGe
translateOp AddrLeOp       = Just mo_wordULe
translateOp AddrGtOp       = Just mo_wordUGt
translateOp AddrLtOp       = Just mo_wordULt

-- Char# ops

translateOp CharEqOp       = Just (MO_Eq wordRep)
translateOp CharNeOp       = Just (MO_Ne wordRep)
translateOp CharGeOp       = Just (MO_U_Ge wordRep)
translateOp CharLeOp       = Just (MO_U_Le wordRep)
translateOp CharGtOp       = Just (MO_U_Gt wordRep)
translateOp CharLtOp       = Just (MO_U_Lt wordRep)

-- Double ops

translateOp DoubleEqOp     = Just (MO_Eq F64)
translateOp DoubleNeOp     = Just (MO_Ne F64)
translateOp DoubleGeOp     = Just (MO_S_Ge F64)
translateOp DoubleLeOp     = Just (MO_S_Le F64)
translateOp DoubleGtOp     = Just (MO_S_Gt F64)
translateOp DoubleLtOp     = Just (MO_S_Lt F64)

translateOp DoubleAddOp    = Just (MO_Add F64)
translateOp DoubleSubOp    = Just (MO_Sub F64)
translateOp DoubleMulOp    = Just (MO_Mul F64)
translateOp DoubleDivOp    = Just (MO_S_Quot F64)
translateOp DoubleNegOp    = Just (MO_S_Neg F64)

-- Float ops

translateOp FloatEqOp     = Just (MO_Eq F32)
translateOp FloatNeOp     = Just (MO_Ne F32)
translateOp FloatGeOp     = Just (MO_S_Ge F32)
translateOp FloatLeOp     = Just (MO_S_Le F32)
translateOp FloatGtOp     = Just (MO_S_Gt F32)
translateOp FloatLtOp     = Just (MO_S_Lt F32)

translateOp FloatAddOp    = Just (MO_Add F32)
translateOp FloatSubOp    = Just (MO_Sub F32)
translateOp FloatMulOp    = Just (MO_Mul F32)
translateOp FloatDivOp    = Just (MO_S_Quot F32)
translateOp FloatNegOp    = Just (MO_S_Neg F32)

-- Conversions

translateOp Int2DoubleOp   = Just (MO_S_Conv wordRep F64)
translateOp Double2IntOp   = Just (MO_S_Conv F64 wordRep)

translateOp Int2FloatOp    = Just (MO_S_Conv wordRep F32)
translateOp Float2IntOp    = Just (MO_S_Conv F32 wordRep)

translateOp Float2DoubleOp = Just (MO_S_Conv F32 F64)
translateOp Double2FloatOp = Just (MO_S_Conv F64 F32)

-- Word comparisons masquerading as more exotic things.

translateOp SameMutVarOp           = Just mo_wordEq
translateOp SameMVarOp             = Just mo_wordEq
translateOp SameMutableArrayOp     = Just mo_wordEq
translateOp SameMutableByteArrayOp = Just mo_wordEq
translateOp SameTVarOp             = Just mo_wordEq
translateOp EqStablePtrOp          = Just mo_wordEq

translateOp _ = Nothing

-- These primops are implemented by CallishMachOps, because they sometimes
-- turn into foreign calls depending on the backend.

callishOp DoublePowerOp  = Just MO_F64_Pwr
callishOp DoubleSinOp    = Just MO_F64_Sin
callishOp DoubleCosOp    = Just MO_F64_Cos
callishOp DoubleTanOp    = Just MO_F64_Tan
callishOp DoubleSinhOp   = Just MO_F64_Sinh
callishOp DoubleCoshOp   = Just MO_F64_Cosh
callishOp DoubleTanhOp   = Just MO_F64_Tanh
callishOp DoubleAsinOp   = Just MO_F64_Asin
callishOp DoubleAcosOp   = Just MO_F64_Acos
callishOp DoubleAtanOp   = Just MO_F64_Atan
callishOp DoubleLogOp    = Just MO_F64_Log
callishOp DoubleExpOp    = Just MO_F64_Exp
callishOp DoubleSqrtOp   = Just MO_F64_Sqrt

callishOp FloatPowerOp  = Just MO_F32_Pwr
callishOp FloatSinOp    = Just MO_F32_Sin
callishOp FloatCosOp    = Just MO_F32_Cos
callishOp FloatTanOp    = Just MO_F32_Tan
callishOp FloatSinhOp   = Just MO_F32_Sinh
callishOp FloatCoshOp   = Just MO_F32_Cosh
callishOp FloatTanhOp   = Just MO_F32_Tanh
callishOp FloatAsinOp   = Just MO_F32_Asin
callishOp FloatAcosOp   = Just MO_F32_Acos
callishOp FloatAtanOp   = Just MO_F32_Atan
callishOp FloatLogOp    = Just MO_F32_Log
callishOp FloatExpOp    = Just MO_F32_Exp
callishOp FloatSqrtOp   = Just MO_F32_Sqrt

callishOp _ = Nothing

------------------------------------------------------------------------------
-- Helpers for translating various minor variants of array indexing.

doIndexOffAddrOp maybe_post_read_cast rep [res] [addr,idx]
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr idx
doIndexOffAddrOp _ _ _ _
   = panic "CgPrimOp: doIndexOffAddrOp"

doIndexByteArrayOp maybe_post_read_cast rep [res] [addr,idx]
   = mkBasicIndexedRead arrWordsHdrSize maybe_post_read_cast rep res addr idx
doIndexByteArrayOp _ _ _ _ 
   = panic "CgPrimOp: doIndexByteArrayOp"

doReadPtrArrayOp res addr idx
   = mkBasicIndexedRead arrPtrsHdrSize Nothing wordRep res addr idx


doWriteOffAddrOp maybe_pre_write_cast rep [] [addr,idx,val]
   = mkBasicIndexedWrite 0 maybe_pre_write_cast rep addr idx val
doWriteOffAddrOp _ _ _ _
   = panic "CgPrimOp: doWriteOffAddrOp"

doWriteByteArrayOp maybe_pre_write_cast rep [] [addr,idx,val]
   = mkBasicIndexedWrite arrWordsHdrSize maybe_pre_write_cast rep addr idx val
doWriteByteArrayOp _ _ _ _ 
   = panic "CgPrimOp: doWriteByteArrayOp"

doWritePtrArrayOp addr idx val
   = do stmtC (setInfo addr (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))
        mkBasicIndexedWrite arrPtrsHdrSize Nothing wordRep addr idx val


mkBasicIndexedRead off Nothing read_rep res base idx
   = stmtC (CmmAssign res (cmmLoadIndexOffExpr off read_rep base idx))
mkBasicIndexedRead off (Just cast) read_rep res base idx
   = stmtC (CmmAssign res (CmmMachOp cast [
				cmmLoadIndexOffExpr off read_rep base idx]))

mkBasicIndexedWrite off Nothing write_rep base idx val
   = stmtC (CmmStore (cmmIndexOffExpr off write_rep base idx) val)
mkBasicIndexedWrite off (Just cast) write_rep base idx val
   = stmtC (CmmStore (cmmIndexOffExpr off write_rep base idx) (CmmMachOp cast [val]))

-- ----------------------------------------------------------------------------
-- Misc utils

cmmIndexOffExpr :: ByteOff -> MachRep -> CmmExpr -> CmmExpr -> CmmExpr
cmmIndexOffExpr off rep base idx
   = cmmIndexExpr rep (cmmOffsetB base off) idx

cmmLoadIndexOffExpr :: ByteOff -> MachRep -> CmmExpr -> CmmExpr -> CmmExpr
cmmLoadIndexOffExpr off rep base idx
   = CmmLoad (cmmIndexOffExpr off rep base idx) rep

setInfo :: CmmExpr -> CmmExpr -> CmmStmt
setInfo closure_ptr info_ptr = CmmStore closure_ptr info_ptr

