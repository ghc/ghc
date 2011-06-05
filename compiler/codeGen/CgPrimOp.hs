-----------------------------------------------------------------------------
--
-- Code generation for PrimOps.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module CgPrimOp (
   cgPrimOp
 ) where

import ForeignCall
import ClosureInfo
import StgSyn
import CgForeignCall
import CgBindery
import CgMonad
import CgInfoTbls
import CgUtils
import OldCmm
import CLabel
import OldCmmUtils
import PrimOp
import SMRep
import Module
import Constants
import Outputable
import FastString

-- ---------------------------------------------------------------------------
-- Code generation for PrimOps

cgPrimOp   :: CmmFormals	-- where to put the results
	   -> PrimOp		-- the op
	   -> [StgArg]		-- arguments
	   -> StgLiveVars	-- live vars, in case we need to save them
	   -> Code

cgPrimOp results op args live
  = do arg_exprs <- getArgAmodes args
       let non_void_args = [ e | (r,e) <- arg_exprs, nonVoidArg r ] 
       emitPrimOp results op non_void_args live


emitPrimOp :: CmmFormals	-- where to put the results
	   -> PrimOp		-- the op
	   -> [CmmExpr]		-- arguments
	   -> StgLiveVars	-- live vars, in case we need to save them
	   -> Code

--  First we handle various awkward cases specially.  The remaining
-- easy cases are then handled by translateOp, defined below.

emitPrimOp [res_r,res_c] IntAddCOp [aa,bb] _
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
        CmmAssign (CmmLocal res_r) (CmmMachOp mo_wordAdd [aa,bb]),
        CmmAssign (CmmLocal res_c) $
	  CmmMachOp mo_wordUShr [
		CmmMachOp mo_wordAnd [
		    CmmMachOp mo_wordNot [CmmMachOp mo_wordXor [aa,bb]],
		    CmmMachOp mo_wordXor [aa, CmmReg (CmmLocal res_r)]
		], 
	        CmmLit (mkIntCLit (wORD_SIZE_IN_BITS - 1))
	  ]
     ]


emitPrimOp [res_r,res_c] IntSubCOp [aa,bb] _
{- Similarly:
   #define subIntCzh(r,c,a,b)					\
   { r = ((I_)(a)) - ((I_)(b));					\
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))	\
         >> (BITS_IN (I_) - 1);					\
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)
-}
   = stmtsC [
        CmmAssign (CmmLocal res_r) (CmmMachOp mo_wordSub [aa,bb]),
        CmmAssign (CmmLocal res_c) $
	  CmmMachOp mo_wordUShr [
		CmmMachOp mo_wordAnd [
		    CmmMachOp mo_wordXor [aa,bb],
		    CmmMachOp mo_wordXor [aa, CmmReg (CmmLocal res_r)]
		], 
	        CmmLit (mkIntCLit (wORD_SIZE_IN_BITS - 1))
	  ]
     ]


emitPrimOp [res] ParOp [arg] live
  = do
	-- for now, just implement this in a C function
	-- later, we might want to inline it.
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
	[CmmHinted res NoHint]
    	(CmmCallee newspark CCallConv) 
	[   (CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint)
          , (CmmHinted arg AddrHint)  ] 
	(Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn
  where
	newspark = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "newSpark")))

emitPrimOp [res] ReadMutVarOp [mutv] _
   = stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexW mutv fixedHdrSize gcWord))

emitPrimOp [] WriteMutVarOp [mutv,val] live
   = do
	vols <- getVolatileRegs live
        join <- newLabelC

        -- store the value in a register, in case it is a non-trivial expression
        tmpl <- newTemp gcWord
        let tmp = CmmReg (CmmLocal tmpl)
        stmtC (CmmAssign (CmmLocal tmpl) val)

        -- save the address of the mut var in a temporary, becuase it may not
        -- be present in 'live' and hence won't be automatically saved across
        -- the foreign call below (live only contains live-in-alts, not
        -- live-in-whole-case, when the primop is the scrutinee of a case).
        mutl <- newTemp gcWord
        let mut = CmmReg (CmmLocal mutl)
        stmtC (CmmAssign (CmmLocal mutl) mutv)

        stmtC (CmmCondBranch (CmmMachOp mo_wordNe [
                                 closureInfoPtr mut,
                                 CmmLit (CmmLabel mkMUT_VAR_GLOBAL_infoLabel) ])
                            join)
	emitForeignCall' PlayRisky
		[CmmHinted tmpl AddrHint]
		(CmmCallee (CmmLit (CmmLabel mkDirty_MUT_VAR_Label)) CCallConv)
		[   (CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint)
                  , (CmmHinted tmp AddrHint)  ]
		(Just vols)
                NoC_SRT -- No SRT b/c we do PlayRisky
                CmmMayReturn
        labelC join
	stmtC (CmmStore (cmmOffsetW mut fixedHdrSize) tmp)

--  #define sizzeofByteArrayzh(r,a) \
--     r = ((StgArrWords *)(a))->bytes
emitPrimOp [res] SizeofByteArrayOp [arg] _
   = stmtC $
	CmmAssign (CmmLocal res) (cmmLoadIndexW arg fixedHdrSize bWord)

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrWords *)(a))->bytes
emitPrimOp [res] SizeofMutableByteArrayOp [arg] live
   = emitPrimOp [res] SizeofByteArrayOp [arg] live


--  #define touchzh(o)                  /* nothing */
emitPrimOp [] TouchOp [_] _
   = nopC

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
emitPrimOp [res] ByteArrayContents_Char [arg] _
   = stmtC (CmmAssign (CmmLocal res) (cmmOffsetB arg arrWordsHdrSize))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
emitPrimOp [res] StableNameToIntOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexW arg fixedHdrSize bWord))

--  #define eqStableNamezh(r,sn1,sn2)					\
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
emitPrimOp [res] EqStableNameOp [arg1,arg2] _
   = stmtC (CmmAssign (CmmLocal res) (CmmMachOp mo_wordEq [
				cmmLoadIndexW arg1 fixedHdrSize bWord,
				cmmLoadIndexW arg2 fixedHdrSize bWord
			 ]))


emitPrimOp [res] ReallyUnsafePtrEqualityOp [arg1,arg2] _
   = stmtC (CmmAssign (CmmLocal res) (CmmMachOp mo_wordEq [arg1,arg2]))

--  #define addrToHValuezh(r,a) r=(P_)a
emitPrimOp [res] AddrToHValueOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) arg)

--  #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
--  Note: argument may be tagged!
emitPrimOp [res] DataToTagOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) (getConstrTag (cmmUntag arg)))

{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.	 -}

--  #define unsafeFreezzeArrayzh(r,a)
--	{
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN0_info);
--	  r = a;
--	}
emitPrimOp [res] UnsafeFreezeArrayOp [arg] _
   = stmtsC [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
	     CmmAssign (CmmLocal res) arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)	r=(a)
emitPrimOp [res] UnsafeFreezeByteArrayOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) arg)

-- Reading/writing pointer arrays

emitPrimOp [r] ReadArrayOp  [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp [r] IndexArrayOp [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp []  WriteArrayOp [obj,ix,v] live  = doWritePtrArrayOp obj ix v live

emitPrimOp [res] SizeofArrayOp [arg] _
   = stmtC $ CmmAssign (CmmLocal res) (cmmLoadIndexW arg (fixedHdrSize + oFFSET_StgMutArrPtrs_ptrs) bWord)
emitPrimOp [res] SizeofMutableArrayOp [arg] live
   = emitPrimOp [res] SizeofArrayOp [arg] live

-- IndexXXXoffAddr

emitPrimOp res IndexOffAddrOp_Char      args _ = doIndexOffAddrOp (Just mo_u_8ToWord) b8 res args
emitPrimOp res IndexOffAddrOp_WideChar  args _ = doIndexOffAddrOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res IndexOffAddrOp_Int       args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res IndexOffAddrOp_Word      args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res IndexOffAddrOp_Addr      args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res IndexOffAddrOp_Float     args _ = doIndexOffAddrOp Nothing f32 res args
emitPrimOp res IndexOffAddrOp_Double    args _ = doIndexOffAddrOp Nothing f64 res args
emitPrimOp res IndexOffAddrOp_StablePtr args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res IndexOffAddrOp_Int8      args _ = doIndexOffAddrOp (Just mo_s_8ToWord)  b8  res args
emitPrimOp res IndexOffAddrOp_Int16     args _ = doIndexOffAddrOp (Just mo_s_16ToWord) b16 res args
emitPrimOp res IndexOffAddrOp_Int32     args _ = doIndexOffAddrOp (Just mo_s_32ToWord) b32 res args
emitPrimOp res IndexOffAddrOp_Int64     args _ = doIndexOffAddrOp Nothing b64 res args
emitPrimOp res IndexOffAddrOp_Word8     args _ = doIndexOffAddrOp (Just mo_u_8ToWord) b8   res args
emitPrimOp res IndexOffAddrOp_Word16    args _ = doIndexOffAddrOp (Just mo_u_16ToWord) b16 res args
emitPrimOp res IndexOffAddrOp_Word32    args _ = doIndexOffAddrOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res IndexOffAddrOp_Word64    args _ = doIndexOffAddrOp Nothing b64 res args

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

emitPrimOp res ReadOffAddrOp_Char      args _ = doIndexOffAddrOp (Just mo_u_8ToWord) b8 res args
emitPrimOp res ReadOffAddrOp_WideChar  args _ = doIndexOffAddrOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res ReadOffAddrOp_Int       args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res ReadOffAddrOp_Word      args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res ReadOffAddrOp_Addr      args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res ReadOffAddrOp_Float     args _ = doIndexOffAddrOp Nothing f32 res args
emitPrimOp res ReadOffAddrOp_Double    args _ = doIndexOffAddrOp Nothing f64 res args
emitPrimOp res ReadOffAddrOp_StablePtr args _ = doIndexOffAddrOp Nothing bWord res args
emitPrimOp res ReadOffAddrOp_Int8      args _ = doIndexOffAddrOp (Just mo_s_8ToWord) b8  res args
emitPrimOp res ReadOffAddrOp_Int16     args _ = doIndexOffAddrOp (Just mo_s_16ToWord) b16 res args
emitPrimOp res ReadOffAddrOp_Int32     args _ = doIndexOffAddrOp (Just mo_s_32ToWord) b32 res args
emitPrimOp res ReadOffAddrOp_Int64     args _ = doIndexOffAddrOp Nothing b64 res args
emitPrimOp res ReadOffAddrOp_Word8     args _ = doIndexOffAddrOp (Just mo_u_8ToWord) b8  res args
emitPrimOp res ReadOffAddrOp_Word16    args _ = doIndexOffAddrOp (Just mo_u_16ToWord) b16 res args
emitPrimOp res ReadOffAddrOp_Word32    args _ = doIndexOffAddrOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res ReadOffAddrOp_Word64    args _ = doIndexOffAddrOp Nothing b64 res args

-- IndexXXXArray

emitPrimOp res IndexByteArrayOp_Char      args _ = doIndexByteArrayOp (Just mo_u_8ToWord) b8 res args
emitPrimOp res IndexByteArrayOp_WideChar  args _ = doIndexByteArrayOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res IndexByteArrayOp_Int       args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res IndexByteArrayOp_Word      args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res IndexByteArrayOp_Addr      args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res IndexByteArrayOp_Float     args _ = doIndexByteArrayOp Nothing f32 res args
emitPrimOp res IndexByteArrayOp_Double    args _ = doIndexByteArrayOp Nothing f64 res args
emitPrimOp res IndexByteArrayOp_StablePtr args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res IndexByteArrayOp_Int8      args _ = doIndexByteArrayOp (Just mo_s_8ToWord) b8  res args
emitPrimOp res IndexByteArrayOp_Int16     args _ = doIndexByteArrayOp (Just mo_s_16ToWord) b16  res args
emitPrimOp res IndexByteArrayOp_Int32     args _ = doIndexByteArrayOp (Just mo_s_32ToWord) b32  res args
emitPrimOp res IndexByteArrayOp_Int64     args _ = doIndexByteArrayOp Nothing b64  res args
emitPrimOp res IndexByteArrayOp_Word8     args _ = doIndexByteArrayOp (Just mo_u_8ToWord) b8  res args
emitPrimOp res IndexByteArrayOp_Word16    args _ = doIndexByteArrayOp (Just mo_u_16ToWord) b16  res args
emitPrimOp res IndexByteArrayOp_Word32    args _ = doIndexByteArrayOp (Just mo_u_32ToWord) b32  res args
emitPrimOp res IndexByteArrayOp_Word64    args _ = doIndexByteArrayOp Nothing b64  res args

-- ReadXXXArray, identical to IndexXXXArray.

emitPrimOp res ReadByteArrayOp_Char       args _ = doIndexByteArrayOp (Just mo_u_8ToWord) b8 res args
emitPrimOp res ReadByteArrayOp_WideChar   args _ = doIndexByteArrayOp (Just mo_u_32ToWord) b32 res args
emitPrimOp res ReadByteArrayOp_Int        args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res ReadByteArrayOp_Word       args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res ReadByteArrayOp_Addr       args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res ReadByteArrayOp_Float      args _ = doIndexByteArrayOp Nothing f32 res args
emitPrimOp res ReadByteArrayOp_Double     args _ = doIndexByteArrayOp Nothing f64 res args
emitPrimOp res ReadByteArrayOp_StablePtr  args _ = doIndexByteArrayOp Nothing bWord res args
emitPrimOp res ReadByteArrayOp_Int8       args _ = doIndexByteArrayOp (Just mo_s_8ToWord) b8  res args
emitPrimOp res ReadByteArrayOp_Int16      args _ = doIndexByteArrayOp (Just mo_s_16ToWord) b16  res args
emitPrimOp res ReadByteArrayOp_Int32      args _ = doIndexByteArrayOp (Just mo_s_32ToWord) b32  res args
emitPrimOp res ReadByteArrayOp_Int64      args _ = doIndexByteArrayOp Nothing b64  res args
emitPrimOp res ReadByteArrayOp_Word8      args _ = doIndexByteArrayOp (Just mo_u_8ToWord) b8  res args
emitPrimOp res ReadByteArrayOp_Word16     args _ = doIndexByteArrayOp (Just mo_u_16ToWord) b16  res args
emitPrimOp res ReadByteArrayOp_Word32     args _ = doIndexByteArrayOp (Just mo_u_32ToWord) b32  res args
emitPrimOp res ReadByteArrayOp_Word64     args _ = doIndexByteArrayOp Nothing b64  res args

-- WriteXXXoffAddr

emitPrimOp res WriteOffAddrOp_Char       args _ = doWriteOffAddrOp (Just mo_WordTo8) b8 res args
emitPrimOp res WriteOffAddrOp_WideChar   args _ = doWriteOffAddrOp (Just mo_WordTo32) b32 res args
emitPrimOp res WriteOffAddrOp_Int        args _ = doWriteOffAddrOp Nothing bWord res args
emitPrimOp res WriteOffAddrOp_Word       args _ = doWriteOffAddrOp Nothing bWord res args
emitPrimOp res WriteOffAddrOp_Addr       args _ = doWriteOffAddrOp Nothing bWord res args
emitPrimOp res WriteOffAddrOp_Float      args _ = doWriteOffAddrOp Nothing f32 res args
emitPrimOp res WriteOffAddrOp_Double     args _ = doWriteOffAddrOp Nothing f64 res args
emitPrimOp res WriteOffAddrOp_StablePtr  args _ = doWriteOffAddrOp Nothing bWord res args
emitPrimOp res WriteOffAddrOp_Int8       args _ = doWriteOffAddrOp (Just mo_WordTo8) b8  res args
emitPrimOp res WriteOffAddrOp_Int16      args _ = doWriteOffAddrOp (Just mo_WordTo16) b16 res args
emitPrimOp res WriteOffAddrOp_Int32      args _ = doWriteOffAddrOp (Just mo_WordTo32) b32 res args
emitPrimOp res WriteOffAddrOp_Int64      args _ = doWriteOffAddrOp Nothing b64 res args
emitPrimOp res WriteOffAddrOp_Word8      args _ = doWriteOffAddrOp (Just mo_WordTo8) b8  res args
emitPrimOp res WriteOffAddrOp_Word16     args _ = doWriteOffAddrOp (Just mo_WordTo16) b16 res args
emitPrimOp res WriteOffAddrOp_Word32     args _ = doWriteOffAddrOp (Just mo_WordTo32) b32 res args
emitPrimOp res WriteOffAddrOp_Word64     args _ = doWriteOffAddrOp Nothing b64 res args

-- WriteXXXArray

emitPrimOp res WriteByteArrayOp_Char      args _ = doWriteByteArrayOp (Just mo_WordTo8) b8 res args
emitPrimOp res WriteByteArrayOp_WideChar  args _ = doWriteByteArrayOp (Just mo_WordTo32) b32 res args
emitPrimOp res WriteByteArrayOp_Int       args _ = doWriteByteArrayOp Nothing bWord res args
emitPrimOp res WriteByteArrayOp_Word      args _ = doWriteByteArrayOp Nothing bWord res args
emitPrimOp res WriteByteArrayOp_Addr      args _ = doWriteByteArrayOp Nothing bWord res args
emitPrimOp res WriteByteArrayOp_Float     args _ = doWriteByteArrayOp Nothing f32 res args
emitPrimOp res WriteByteArrayOp_Double    args _ = doWriteByteArrayOp Nothing f64 res args
emitPrimOp res WriteByteArrayOp_StablePtr args _ = doWriteByteArrayOp Nothing bWord res args
emitPrimOp res WriteByteArrayOp_Int8      args _ = doWriteByteArrayOp (Just mo_WordTo8) b8  res args
emitPrimOp res WriteByteArrayOp_Int16     args _ = doWriteByteArrayOp (Just mo_WordTo16) b16  res args
emitPrimOp res WriteByteArrayOp_Int32     args _ = doWriteByteArrayOp (Just mo_WordTo32) b32  res args
emitPrimOp res WriteByteArrayOp_Int64     args _ = doWriteByteArrayOp Nothing b64  res args
emitPrimOp res WriteByteArrayOp_Word8     args _ = doWriteByteArrayOp (Just mo_WordTo8) b8  res args
emitPrimOp res WriteByteArrayOp_Word16    args _ = doWriteByteArrayOp (Just mo_WordTo16) b16  res args
emitPrimOp res WriteByteArrayOp_Word32    args _ = doWriteByteArrayOp (Just mo_WordTo32) b32  res args
emitPrimOp res WriteByteArrayOp_Word64    args _ = doWriteByteArrayOp Nothing b64  res args


-- The rest just translate straightforwardly
emitPrimOp [res] op [arg] _
   | nopOp op
   = stmtC (CmmAssign (CmmLocal res) arg)

   | Just (mop,rep) <- narrowOp op
   = stmtC (CmmAssign (CmmLocal res) $
	    CmmMachOp (mop rep wordWidth) [CmmMachOp (mop wordWidth rep) [arg]])

emitPrimOp [res] op args live
   | Just prim <- callishOp op
   = do vols <- getVolatileRegs live
	emitForeignCall' PlayRisky
	   [CmmHinted res NoHint] 
	   (CmmPrim prim) 
	   [CmmHinted a NoHint | a<-args]  -- ToDo: hints?
	   (Just vols)
           NoC_SRT -- No SRT b/c we do PlayRisky
           CmmMayReturn

   | Just mop <- translateOp op
   = let stmt = CmmAssign (CmmLocal res) (CmmMachOp mop args) in
     stmtC stmt

emitPrimOp _ op _ _
 = pprPanic "emitPrimOp: can't translate PrimOp" (ppr op)


-- These PrimOps are NOPs in Cmm

nopOp :: PrimOp -> Bool
nopOp Int2WordOp     = True
nopOp Word2IntOp     = True
nopOp Int2AddrOp     = True
nopOp Addr2IntOp     = True
nopOp ChrOp	     = True  -- Int# and Char# are rep'd the same
nopOp OrdOp	     = True
nopOp _		     = False

-- These PrimOps turn into double casts

narrowOp :: PrimOp -> Maybe (Width -> Width -> MachOp, Width)
narrowOp Narrow8IntOp   = Just (MO_SS_Conv, W8)
narrowOp Narrow16IntOp  = Just (MO_SS_Conv, W16)
narrowOp Narrow32IntOp  = Just (MO_SS_Conv, W32)
narrowOp Narrow8WordOp  = Just (MO_UU_Conv, W8)
narrowOp Narrow16WordOp = Just (MO_UU_Conv, W16)
narrowOp Narrow32WordOp = Just (MO_UU_Conv, W32)
narrowOp _ 		= Nothing

-- Native word signless ops

translateOp :: PrimOp -> Maybe MachOp
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
translateOp IntMulMayOfloOp = Just (MO_S_MulMayOflo wordWidth)
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

translateOp CharEqOp       = Just (MO_Eq wordWidth)
translateOp CharNeOp       = Just (MO_Ne wordWidth)
translateOp CharGeOp       = Just (MO_U_Ge wordWidth)
translateOp CharLeOp       = Just (MO_U_Le wordWidth)
translateOp CharGtOp       = Just (MO_U_Gt wordWidth)
translateOp CharLtOp       = Just (MO_U_Lt wordWidth)

-- Double ops

translateOp DoubleEqOp     = Just (MO_F_Eq W64)
translateOp DoubleNeOp     = Just (MO_F_Ne W64)
translateOp DoubleGeOp     = Just (MO_F_Ge W64)
translateOp DoubleLeOp     = Just (MO_F_Le W64)
translateOp DoubleGtOp     = Just (MO_F_Gt W64)
translateOp DoubleLtOp     = Just (MO_F_Lt W64)

translateOp DoubleAddOp    = Just (MO_F_Add W64)
translateOp DoubleSubOp    = Just (MO_F_Sub W64)
translateOp DoubleMulOp    = Just (MO_F_Mul W64)
translateOp DoubleDivOp    = Just (MO_F_Quot W64)
translateOp DoubleNegOp    = Just (MO_F_Neg W64)

-- Float ops

translateOp FloatEqOp     = Just (MO_F_Eq W32)
translateOp FloatNeOp     = Just (MO_F_Ne W32)
translateOp FloatGeOp     = Just (MO_F_Ge W32)
translateOp FloatLeOp     = Just (MO_F_Le W32)
translateOp FloatGtOp     = Just (MO_F_Gt W32)
translateOp FloatLtOp     = Just (MO_F_Lt W32)

translateOp FloatAddOp    = Just (MO_F_Add  W32)
translateOp FloatSubOp    = Just (MO_F_Sub  W32)
translateOp FloatMulOp    = Just (MO_F_Mul  W32)
translateOp FloatDivOp    = Just (MO_F_Quot W32)
translateOp FloatNegOp    = Just (MO_F_Neg  W32)

-- Conversions

translateOp Int2DoubleOp   = Just (MO_SF_Conv wordWidth W64)
translateOp Double2IntOp   = Just (MO_FS_Conv W64 wordWidth)

translateOp Int2FloatOp    = Just (MO_SF_Conv wordWidth W32)
translateOp Float2IntOp    = Just (MO_FS_Conv W32 wordWidth)

translateOp Float2DoubleOp = Just (MO_FF_Conv W32 W64)
translateOp Double2FloatOp = Just (MO_FF_Conv W64 W32)

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

callishOp :: PrimOp -> Maybe CallishMachOp
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

-- Bytearrays outside the heap; hence non-pointers
doIndexOffAddrOp, doIndexByteArrayOp 
	:: Maybe MachOp -> CmmType 
	-> [LocalReg] -> [CmmExpr] -> Code
doIndexOffAddrOp maybe_post_read_cast rep [res] [addr,idx]
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr idx
doIndexOffAddrOp _ _ _ _
   = panic "CgPrimOp: doIndexOffAddrOp"

doIndexByteArrayOp maybe_post_read_cast rep [res] [addr,idx]
   = mkBasicIndexedRead arrWordsHdrSize maybe_post_read_cast rep res addr idx
doIndexByteArrayOp _ _ _ _ 
   = panic "CgPrimOp: doIndexByteArrayOp"

doReadPtrArrayOp :: LocalReg -> CmmExpr -> CmmExpr -> Code
doReadPtrArrayOp res addr idx
   = mkBasicIndexedRead arrPtrsHdrSize Nothing gcWord res addr idx


doWriteOffAddrOp, doWriteByteArrayOp 
	:: Maybe MachOp -> CmmType 
	-> [LocalReg] -> [CmmExpr] -> Code
doWriteOffAddrOp maybe_pre_write_cast rep [] [addr,idx,val]
   = mkBasicIndexedWrite 0 maybe_pre_write_cast rep addr idx val
doWriteOffAddrOp _ _ _ _
   = panic "CgPrimOp: doWriteOffAddrOp"

doWriteByteArrayOp maybe_pre_write_cast rep [] [addr,idx,val]
   = mkBasicIndexedWrite arrWordsHdrSize maybe_pre_write_cast rep addr idx val
doWriteByteArrayOp _ _ _ _ 
   = panic "CgPrimOp: doWriteByteArrayOp"

doWritePtrArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> StgLiveVars -> Code
doWritePtrArrayOp addr idx val live
   = do vols <- getVolatileRegs live
        join <- newLabelC

        -- store the value in a register, in case it is a non-trivial expression
        tmpl <- newTemp gcWord
        let tmp = CmmReg (CmmLocal tmpl)
        stmtC (CmmAssign (CmmLocal tmpl) val)

        -- save the address of the array in a temporary, becuase it may not
        -- be present in 'live' and hence won't be automatically saved across
        -- the foreign call below (live only contains live-in-alts, not
        -- live-in-whole-case, when the primop is the scrutinee of a case).
        arrl <- newTemp gcWord
        let arr = CmmReg (CmmLocal arrl)
        stmtC (CmmAssign (CmmLocal arrl) addr)
        stmtC (CmmCondBranch (CmmMachOp mo_wordNe [
                                 closureInfoPtr arr,
                                 CmmLit (CmmLabel mkMAP_GLOBAL_infoLabel) ])
                            join)
	emitForeignCall' PlayRisky
		[CmmHinted tmpl AddrHint]
		(CmmCallee (CmmLit (CmmLabel mkDirty_MUT_ARR_Label)) CCallConv)
		[   (CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint)
                  , (CmmHinted tmp AddrHint) ]
		(Just vols)
                NoC_SRT -- No SRT b/c we do PlayRisky
                CmmMayReturn
        labelC join
        mkBasicIndexedWrite arrPtrsHdrSize Nothing bWord arr idx tmp

--   -- the write barrier.  We must write a byte into the mark table:
--   -- bits8[a + header_size + StgMutArrPtrs_size(a) + x >> N]
--        stmtC (setInfo addr (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))
--        stmtC $ CmmStore (
--          cmmOffsetExpr
--           (cmmOffsetExprW (cmmOffsetB addr arrPtrsHdrSize)
--                          (loadArrPtrsSize addr))
--           (CmmMachOp mo_wordUShr [idx,
--                                   CmmLit (mkIntCLit mUT_ARR_PTRS_CARD_BITS)])
--          ) (CmmLit (CmmInt 1 W8))

loadArrPtrsSize :: CmmExpr -> CmmExpr
loadArrPtrsSize addr = CmmLoad (cmmOffsetB addr off) bWord
 where off = fixedHdrSize*wORD_SIZE + oFFSET_StgMutArrPtrs_ptrs

mkBasicIndexedRead :: ByteOff -> Maybe MachOp -> CmmType 
		   -> LocalReg -> CmmExpr -> CmmExpr -> Code
mkBasicIndexedRead off Nothing read_rep res base idx
   = stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexOffExpr off read_rep base idx))
mkBasicIndexedRead off (Just cast) read_rep res base idx
   = stmtC (CmmAssign (CmmLocal res) (CmmMachOp cast [
				cmmLoadIndexOffExpr off read_rep base idx]))

mkBasicIndexedWrite :: ByteOff -> Maybe MachOp -> CmmType 
		    -> CmmExpr -> CmmExpr -> CmmExpr -> Code
mkBasicIndexedWrite off Nothing write_rep base idx val
   = stmtC (CmmStore (cmmIndexOffExpr off write_rep base idx) val)
mkBasicIndexedWrite off (Just cast) write_rep base idx val
   = stmtC (CmmStore (cmmIndexOffExpr off write_rep base idx) (CmmMachOp cast [val]))

-- ----------------------------------------------------------------------------
-- Misc utils

cmmIndexOffExpr :: ByteOff -> CmmType -> CmmExpr -> CmmExpr -> CmmExpr
cmmIndexOffExpr off rep base idx
   = cmmIndexExpr (typeWidth rep) (cmmOffsetB base off) idx

cmmLoadIndexOffExpr :: ByteOff -> CmmType -> CmmExpr -> CmmExpr -> CmmExpr
cmmLoadIndexOffExpr off rep base idx
   = CmmLoad (cmmIndexOffExpr off rep base idx) rep

setInfo :: CmmExpr -> CmmExpr -> CmmStmt
setInfo closure_ptr info_ptr = CmmStore closure_ptr info_ptr

