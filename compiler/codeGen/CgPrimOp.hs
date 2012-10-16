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

import BasicTypes
import ForeignCall
import ClosureInfo
import StgSyn
import CgForeignCall
import CgBindery
import CgMonad
import CgHeapery
import CgInfoTbls
import CgTicky
import CgProf
import CgUtils
import OldCmm
import CLabel
import OldCmmUtils
import PrimOp
import SMRep
import Module
import Outputable
import DynFlags
import FastString

import Control.Monad
import Data.Bits

-- ---------------------------------------------------------------------------
-- Code generation for PrimOps

cgPrimOp :: [CmmFormal]       -- where to put the results
         -> PrimOp            -- the op
         -> [StgArg]          -- arguments
         -> StgLiveVars       -- live vars, in case we need to save them
         -> Code

cgPrimOp results op args live
  = do dflags <- getDynFlags
       arg_exprs <- getArgAmodes args
       let non_void_args = [ e | (r,e) <- arg_exprs, nonVoidArg r ]
       emitPrimOp dflags results op non_void_args live


emitPrimOp :: DynFlags
           -> [CmmFormal]       -- where to put the results
           -> PrimOp            -- the op
           -> [CmmExpr]         -- arguments
           -> StgLiveVars       -- live vars, in case we need to save them
           -> Code

--  First we handle various awkward cases specially.  The remaining
-- easy cases are then handled by translateOp, defined below.

emitPrimOp dflags [res_r,res_c] IntAddCOp [aa,bb] _
{-
   With some bit-twiddling, we can define int{Add,Sub}Czh portably in
   C, and without needing any comparisons.  This may not be the
   fastest way to do it - if you have better code, please send it! --SDM

   Return : r = a + b,  c = 0 if no overflow, 1 on overflow.

   We currently don't make use of the r value if c is != 0 (i.e.
   overflow), we just convert to big integers and try again.  This
   could be improved by making r and c the correct values for
   plugging into a new J#.

   { r = ((I_)(a)) + ((I_)(b));                                 \
     c = ((StgWord)(~(((I_)(a))^((I_)(b))) & (((I_)(a))^r)))    \
         >> (BITS_IN (I_) - 1);                                 \
   }
   Wading through the mass of bracketry, it seems to reduce to:
   c = ( (~(a^b)) & (a^r) ) >>unsigned (BITS_IN(I_)-1)

-}
   = stmtsC [
        CmmAssign (CmmLocal res_r) (CmmMachOp (mo_wordAdd dflags) [aa,bb]),
        CmmAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr dflags) [
                CmmMachOp (mo_wordAnd dflags) [
                    CmmMachOp (mo_wordNot dflags) [CmmMachOp (mo_wordXor dflags) [aa,bb]],
                    CmmMachOp (mo_wordXor dflags) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr dflags (wORD_SIZE_IN_BITS dflags - 1)
          ]
     ]


emitPrimOp dflags [res_r,res_c] IntSubCOp [aa,bb] _
{- Similarly:
   #define subIntCzh(r,c,a,b)                                   \
   { r = ((I_)(a)) - ((I_)(b));                                 \
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))     \
         >> (BITS_IN (I_) - 1);                                 \
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)
-}
   = stmtsC [
        CmmAssign (CmmLocal res_r) (CmmMachOp (mo_wordSub dflags) [aa,bb]),
        CmmAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr dflags) [
                CmmMachOp (mo_wordAnd dflags) [
                    CmmMachOp (mo_wordXor dflags) [aa,bb],
                    CmmMachOp (mo_wordXor dflags) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr dflags (wORD_SIZE_IN_BITS dflags - 1)
          ]
     ]


emitPrimOp _      [res] ParOp [arg] live
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

emitPrimOp dflags [res] SparkOp [arg] live = do
    -- returns the value of arg in res.  We're going to therefore
    -- refer to arg twice (once to pass to newSpark(), and once to
    -- assign to res), so put it in a temporary.
    tmp <- newTemp (bWord dflags)
    stmtC (CmmAssign (CmmLocal tmp) arg)

    vols <- getVolatileRegs live
    res' <- newTemp (bWord dflags)
    emitForeignCall' PlayRisky
        [CmmHinted res' NoHint]
        (CmmCallee newspark CCallConv)
        [   (CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint)
          , (CmmHinted arg AddrHint)  ]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn
    stmtC (CmmAssign (CmmLocal res) (CmmReg (CmmLocal tmp)))
  where
        newspark = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "newSpark")))

emitPrimOp dflags [res] GetCCSOfOp [arg] _live
  = stmtC (CmmAssign (CmmLocal res) val)
  where
    val
     | gopt Opt_SccProfilingOn dflags = costCentreFrom dflags (cmmUntag dflags arg)
     | otherwise                      = CmmLit (zeroCLit dflags)

emitPrimOp _      [res] GetCurrentCCSOp [_dummy_arg] _live
   = stmtC (CmmAssign (CmmLocal res) curCCS)

emitPrimOp dflags [res] ReadMutVarOp [mutv] _
   = stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexW dflags mutv (fixedHdrSize dflags) (gcWord dflags)))

emitPrimOp dflags [] WriteMutVarOp [mutv,var] live
   = do stmtC (CmmStore (cmmOffsetW dflags mutv (fixedHdrSize dflags)) var)
        vols <- getVolatileRegs live
        emitForeignCall' PlayRisky
                [{-no results-}]
                (CmmCallee (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
                         CCallConv)
                [   (CmmHinted (CmmReg (CmmGlobal BaseReg)) AddrHint)
                  , (CmmHinted mutv AddrHint)  ]
                (Just vols)
                NoC_SRT -- No SRT b/c we do PlayRisky
                CmmMayReturn

--  #define sizzeofByteArrayzh(r,a) \
--     r = ((StgArrWords *)(a))->bytes
emitPrimOp dflags [res] SizeofByteArrayOp [arg] _
   = stmtC $
         CmmAssign (CmmLocal res)
                   (cmmLoadIndexW dflags arg (fixedHdrSize dflags) (bWord dflags))

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrWords *)(a))->bytes
emitPrimOp dflags [res] SizeofMutableByteArrayOp [arg] live
   = emitPrimOp dflags [res] SizeofByteArrayOp [arg] live


--  #define touchzh(o)                  /* nothing */
emitPrimOp _      [] TouchOp [_] _
   = nopC

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
emitPrimOp dflags [res] ByteArrayContents_Char [arg] _
   = stmtC (CmmAssign (CmmLocal res) (cmmOffsetB dflags arg (arrWordsHdrSize dflags)))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
emitPrimOp dflags [res] StableNameToIntOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSize dflags) (bWord dflags)))

--  #define eqStableNamezh(r,sn1,sn2)                                   \
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
emitPrimOp dflags [res] EqStableNameOp [arg1,arg2] _
   = stmtC (CmmAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [
                             cmmLoadIndexW dflags arg1 (fixedHdrSize dflags) (bWord dflags),
                             cmmLoadIndexW dflags arg2 (fixedHdrSize dflags) (bWord dflags)
                      ]))


emitPrimOp dflags [res] ReallyUnsafePtrEqualityOp [arg1,arg2] _
   = stmtC (CmmAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [arg1,arg2]))

--  #define addrToHValuezh(r,a) r=(P_)a
emitPrimOp _      [res] AddrToAnyOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) arg)

--  #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
--  Note: argument may be tagged!
emitPrimOp dflags [res] DataToTagOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) (getConstrTag dflags (cmmUntag dflags arg)))

{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.  -}

--  #define unsafeFreezzeArrayzh(r,a)
--      {
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN0_info);
--        r = a;
--      }
emitPrimOp _      [res] UnsafeFreezeArrayOp [arg] _
   = stmtsC [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
       CmmAssign (CmmLocal res) arg ]
emitPrimOp _      [res] UnsafeFreezeArrayArrayOp [arg] _
   = stmtsC [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
       CmmAssign (CmmLocal res) arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)       r=(a)
emitPrimOp _      [res] UnsafeFreezeByteArrayOp [arg] _
   = stmtC (CmmAssign (CmmLocal res) arg)

emitPrimOp _      [] CopyArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyArrayOp src src_off dst dst_off n live
emitPrimOp _      [] CopyMutableArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyMutableArrayOp src src_off dst dst_off n live
emitPrimOp _      [res] CloneArrayOp [src,src_off,n] live =
    emitCloneArray mkMAP_FROZEN_infoLabel res src src_off n live
emitPrimOp _      [res] CloneMutableArrayOp [src,src_off,n] live =
    emitCloneArray mkMAP_DIRTY_infoLabel res src src_off n live
emitPrimOp _      [res] FreezeArrayOp [src,src_off,n] live =
    emitCloneArray mkMAP_FROZEN_infoLabel res src src_off n live
emitPrimOp _      [res] ThawArrayOp [src,src_off,n] live =
    emitCloneArray mkMAP_DIRTY_infoLabel res src src_off n live

emitPrimOp _      [] CopyArrayArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyArrayOp src src_off dst dst_off n live
emitPrimOp _      [] CopyMutableArrayArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyMutableArrayOp src src_off dst dst_off n live

-- Reading/writing pointer arrays

emitPrimOp _      [r] ReadArrayOp  [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] IndexArrayOp [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      []  WriteArrayOp [obj,ix,v] _  = doWritePtrArrayOp obj ix v

emitPrimOp _      [r] IndexArrayArrayOp_ByteArray         [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] IndexArrayArrayOp_ArrayArray        [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] ReadArrayArrayOp_ByteArray          [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] ReadArrayArrayOp_MutableByteArray   [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] ReadArrayArrayOp_ArrayArray         [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      [r] ReadArrayArrayOp_MutableArrayArray  [obj,ix]   _  = doReadPtrArrayOp r obj ix
emitPrimOp _      []  WriteArrayArrayOp_ByteArray         [obj,ix,v] _  = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_MutableByteArray  [obj,ix,v] _  = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_ArrayArray        [obj,ix,v] _  = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_MutableArrayArray [obj,ix,v] _  = doWritePtrArrayOp obj ix v

emitPrimOp dflags [res] SizeofArrayOp [arg] _
   = stmtC $ CmmAssign (CmmLocal res)
                       (cmmLoadIndexW dflags arg (fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags) (bWord dflags))
emitPrimOp dflags [res] SizeofMutableArrayOp [arg] live
   = emitPrimOp dflags [res] SizeofArrayOp [arg] live
emitPrimOp dflags [res] SizeofArrayArrayOp [arg] live
   = emitPrimOp dflags [res] SizeofArrayOp [arg] live
emitPrimOp dflags [res] SizeofMutableArrayArrayOp [arg] live
   = emitPrimOp dflags [res] SizeofArrayOp [arg] live

-- IndexXXXoffAddr

emitPrimOp dflags res IndexOffAddrOp_Char      args _ = doIndexOffAddrOp (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res IndexOffAddrOp_WideChar  args _ = doIndexOffAddrOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res IndexOffAddrOp_Int       args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Word      args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Addr      args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp _      res IndexOffAddrOp_Float     args _ = doIndexOffAddrOp Nothing f32 res args
emitPrimOp _      res IndexOffAddrOp_Double    args _ = doIndexOffAddrOp Nothing f64 res args
emitPrimOp dflags res IndexOffAddrOp_StablePtr args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Int8      args _ = doIndexOffAddrOp (Just (mo_s_8ToWord dflags))  b8  res args
emitPrimOp dflags res IndexOffAddrOp_Int16     args _ = doIndexOffAddrOp (Just (mo_s_16ToWord dflags)) b16 res args
emitPrimOp dflags res IndexOffAddrOp_Int32     args _ = doIndexOffAddrOp (Just (mo_s_32ToWord dflags)) b32 res args
emitPrimOp _      res IndexOffAddrOp_Int64     args _ = doIndexOffAddrOp Nothing b64 res args
emitPrimOp dflags res IndexOffAddrOp_Word8     args _ = doIndexOffAddrOp (Just (mo_u_8ToWord dflags)) b8   res args
emitPrimOp dflags res IndexOffAddrOp_Word16    args _ = doIndexOffAddrOp (Just (mo_u_16ToWord dflags)) b16 res args
emitPrimOp dflags res IndexOffAddrOp_Word32    args _ = doIndexOffAddrOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp _      res IndexOffAddrOp_Word64    args _ = doIndexOffAddrOp Nothing b64 res args

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

emitPrimOp dflags res ReadOffAddrOp_Char      args _ = doIndexOffAddrOp (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res ReadOffAddrOp_WideChar  args _ = doIndexOffAddrOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res ReadOffAddrOp_Int       args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Word      args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Addr      args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp _      res ReadOffAddrOp_Float     args _ = doIndexOffAddrOp Nothing f32 res args
emitPrimOp _      res ReadOffAddrOp_Double    args _ = doIndexOffAddrOp Nothing f64 res args
emitPrimOp dflags res ReadOffAddrOp_StablePtr args _ = doIndexOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Int8      args _ = doIndexOffAddrOp (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadOffAddrOp_Int16     args _ = doIndexOffAddrOp (Just (mo_s_16ToWord dflags)) b16 res args
emitPrimOp dflags res ReadOffAddrOp_Int32     args _ = doIndexOffAddrOp (Just (mo_s_32ToWord dflags)) b32 res args
emitPrimOp _      res ReadOffAddrOp_Int64     args _ = doIndexOffAddrOp Nothing b64 res args
emitPrimOp dflags res ReadOffAddrOp_Word8     args _ = doIndexOffAddrOp (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadOffAddrOp_Word16    args _ = doIndexOffAddrOp (Just (mo_u_16ToWord dflags)) b16 res args
emitPrimOp dflags res ReadOffAddrOp_Word32    args _ = doIndexOffAddrOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp _      res ReadOffAddrOp_Word64    args _ = doIndexOffAddrOp Nothing b64 res args

-- IndexXXXArray

emitPrimOp dflags res IndexByteArrayOp_Char      args _ = doIndexByteArrayOp (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res IndexByteArrayOp_WideChar  args _ = doIndexByteArrayOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res IndexByteArrayOp_Int       args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Word      args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Addr      args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp _      res IndexByteArrayOp_Float     args _ = doIndexByteArrayOp Nothing f32 res args
emitPrimOp _      res IndexByteArrayOp_Double    args _ = doIndexByteArrayOp Nothing f64 res args
emitPrimOp dflags res IndexByteArrayOp_StablePtr args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Int8      args _ = doIndexByteArrayOp (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexByteArrayOp_Int16     args _ = doIndexByteArrayOp (Just (mo_s_16ToWord dflags)) b16  res args
emitPrimOp dflags res IndexByteArrayOp_Int32     args _ = doIndexByteArrayOp (Just (mo_s_32ToWord dflags)) b32  res args
emitPrimOp _      res IndexByteArrayOp_Int64     args _ = doIndexByteArrayOp Nothing b64  res args
emitPrimOp dflags res IndexByteArrayOp_Word8     args _ = doIndexByteArrayOp (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexByteArrayOp_Word16    args _ = doIndexByteArrayOp (Just (mo_u_16ToWord dflags)) b16  res args
emitPrimOp dflags res IndexByteArrayOp_Word32    args _ = doIndexByteArrayOp (Just (mo_u_32ToWord dflags)) b32  res args
emitPrimOp _      res IndexByteArrayOp_Word64    args _ = doIndexByteArrayOp Nothing b64  res args

-- ReadXXXArray, identical to IndexXXXArray.

emitPrimOp dflags res ReadByteArrayOp_Char       args _ = doIndexByteArrayOp (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res ReadByteArrayOp_WideChar   args _ = doIndexByteArrayOp (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res ReadByteArrayOp_Int        args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Word       args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Addr       args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp _      res ReadByteArrayOp_Float      args _ = doIndexByteArrayOp Nothing f32 res args
emitPrimOp _      res ReadByteArrayOp_Double     args _ = doIndexByteArrayOp Nothing f64 res args
emitPrimOp dflags res ReadByteArrayOp_StablePtr  args _ = doIndexByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Int8       args _ = doIndexByteArrayOp (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadByteArrayOp_Int16      args _ = doIndexByteArrayOp (Just (mo_s_16ToWord dflags)) b16  res args
emitPrimOp dflags res ReadByteArrayOp_Int32      args _ = doIndexByteArrayOp (Just (mo_s_32ToWord dflags)) b32  res args
emitPrimOp _      res ReadByteArrayOp_Int64      args _ = doIndexByteArrayOp Nothing b64  res args
emitPrimOp dflags res ReadByteArrayOp_Word8      args _ = doIndexByteArrayOp (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadByteArrayOp_Word16     args _ = doIndexByteArrayOp (Just (mo_u_16ToWord dflags)) b16  res args
emitPrimOp dflags res ReadByteArrayOp_Word32     args _ = doIndexByteArrayOp (Just (mo_u_32ToWord dflags)) b32  res args
emitPrimOp _      res ReadByteArrayOp_Word64     args _ = doIndexByteArrayOp Nothing b64  res args

-- WriteXXXoffAddr

emitPrimOp dflags res WriteOffAddrOp_Char       args _ = doWriteOffAddrOp (Just (mo_WordTo8 dflags)) b8 res args
emitPrimOp dflags res WriteOffAddrOp_WideChar   args _ = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp dflags res WriteOffAddrOp_Int        args _ = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Word       args _ = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Addr       args _ = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp _      res WriteOffAddrOp_Float      args _ = doWriteOffAddrOp Nothing f32 res args
emitPrimOp _      res WriteOffAddrOp_Double     args _ = doWriteOffAddrOp Nothing f64 res args
emitPrimOp dflags res WriteOffAddrOp_StablePtr  args _ = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Int8       args _ = doWriteOffAddrOp (Just (mo_WordTo8 dflags)) b8  res args
emitPrimOp dflags res WriteOffAddrOp_Int16      args _ = doWriteOffAddrOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteOffAddrOp_Int32      args _ = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteOffAddrOp_Int64      args _ = doWriteOffAddrOp Nothing b64 res args
emitPrimOp dflags res WriteOffAddrOp_Word8      args _ = doWriteOffAddrOp (Just (mo_WordTo8 dflags)) b8  res args
emitPrimOp dflags res WriteOffAddrOp_Word16     args _ = doWriteOffAddrOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteOffAddrOp_Word32     args _ = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteOffAddrOp_Word64     args _ = doWriteOffAddrOp Nothing b64 res args

-- WriteXXXArray

emitPrimOp dflags res WriteByteArrayOp_Char      args _ = doWriteByteArrayOp (Just (mo_WordTo8 dflags)) b8 res args
emitPrimOp dflags res WriteByteArrayOp_WideChar  args _ = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp dflags res WriteByteArrayOp_Int       args _ = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Word      args _ = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Addr      args _ = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp _      res WriteByteArrayOp_Float     args _ = doWriteByteArrayOp Nothing f32 res args
emitPrimOp _      res WriteByteArrayOp_Double    args _ = doWriteByteArrayOp Nothing f64 res args
emitPrimOp dflags res WriteByteArrayOp_StablePtr args _ = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Int8      args _ = doWriteByteArrayOp (Just (mo_WordTo8 dflags)) b8  res args
emitPrimOp dflags res WriteByteArrayOp_Int16     args _ = doWriteByteArrayOp (Just (mo_WordTo16 dflags)) b16  res args
emitPrimOp dflags res WriteByteArrayOp_Int32     args _ = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32  res args
emitPrimOp _      res WriteByteArrayOp_Int64     args _ = doWriteByteArrayOp Nothing b64  res args
emitPrimOp dflags res WriteByteArrayOp_Word8     args _ = doWriteByteArrayOp (Just (mo_WordTo8 dflags)) b8  res args
emitPrimOp dflags res WriteByteArrayOp_Word16    args _ = doWriteByteArrayOp (Just (mo_WordTo16 dflags)) b16  res args
emitPrimOp dflags res WriteByteArrayOp_Word32    args _ = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32  res args
emitPrimOp _      res WriteByteArrayOp_Word64    args _ = doWriteByteArrayOp Nothing b64  res args

-- Copying and setting byte arrays

emitPrimOp _ [] CopyByteArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyByteArrayOp src src_off dst dst_off n live
emitPrimOp _ [] CopyMutableByteArrayOp [src,src_off,dst,dst_off,n] live =
    doCopyMutableByteArrayOp src src_off dst dst_off n live
emitPrimOp _ [] SetByteArrayOp [ba,off,len,c] live =
    doSetByteArrayOp ba off len c live

-- Population count.
-- The type of the primop takes a Word#, so we have to be careful to narrow
-- to the correct width before calling the primop.  Otherwise this can result
-- in a crash e.g. when calling the helper hs_popcnt8() which assumes that the
-- argument is <=0xff.
emitPrimOp dflags [res] PopCnt8Op [w] live =
  emitPopCntCall res (CmmMachOp (mo_WordTo8 dflags) [w]) W8 live
emitPrimOp dflags [res] PopCnt16Op [w] live =
  emitPopCntCall res (CmmMachOp (mo_WordTo16 dflags) [w]) W16 live
emitPrimOp dflags [res] PopCnt32Op [w] live =
  emitPopCntCall res (CmmMachOp (mo_WordTo32 dflags) [w]) W32 live
emitPrimOp dflags [res] PopCnt64Op [w] live =
  emitPopCntCall res (CmmMachOp (mo_WordTo64 dflags) [w]) W64 live
emitPrimOp dflags [res] PopCntOp [w] live =
  emitPopCntCall res w (wordWidth dflags) live

-- The rest just translate straightforwardly
emitPrimOp dflags [res] op [arg] _
   | nopOp op
   = stmtC (CmmAssign (CmmLocal res) arg)

   | Just (mop,rep) <- narrowOp op
   = stmtC (CmmAssign (CmmLocal res) $
            CmmMachOp (mop rep (wordWidth dflags)) [CmmMachOp (mop (wordWidth dflags) rep) [arg]])

emitPrimOp dflags [res] op args live
   | Just prim <- callishOp op
   = do vols <- getVolatileRegs live
        emitForeignCall' PlayRisky
           [CmmHinted res NoHint]
           (CmmPrim prim Nothing)
           [CmmHinted a NoHint | a<-args]  -- ToDo: hints?
           (Just vols)
           NoC_SRT -- No SRT b/c we do PlayRisky
           CmmMayReturn

   | Just mop <- translateOp dflags op
   = let stmt = CmmAssign (CmmLocal res) (CmmMachOp mop args) in
     stmtC stmt

emitPrimOp dflags [res_q, res_r] IntQuotRemOp [arg_x, arg_y] _
    = let genericImpl
              = [CmmAssign (CmmLocal res_q)
                           (CmmMachOp (MO_S_Quot (wordWidth dflags)) [arg_x, arg_y]),
                 CmmAssign (CmmLocal res_r)
                           (CmmMachOp (MO_S_Rem  (wordWidth dflags)) [arg_x, arg_y])]
          stmt = CmmCall (CmmPrim (MO_S_QuotRem (wordWidth dflags)) (Just genericImpl))
                         [CmmHinted res_q NoHint,
                          CmmHinted res_r NoHint]
                         [CmmHinted arg_x NoHint,
                          CmmHinted arg_y NoHint]
                         CmmMayReturn
      in stmtC stmt
emitPrimOp dflags [res_q, res_r] WordQuotRemOp [arg_x, arg_y] _
    = let genericImpl
              = [CmmAssign (CmmLocal res_q)
                           (CmmMachOp (MO_U_Quot (wordWidth dflags)) [arg_x, arg_y]),
                 CmmAssign (CmmLocal res_r)
                           (CmmMachOp (MO_U_Rem  (wordWidth dflags)) [arg_x, arg_y])]
          stmt = CmmCall (CmmPrim (MO_U_QuotRem (wordWidth dflags)) (Just genericImpl))
                         [CmmHinted res_q NoHint,
                          CmmHinted res_r NoHint]
                         [CmmHinted arg_x NoHint,
                          CmmHinted arg_y NoHint]
                         CmmMayReturn
      in stmtC stmt
emitPrimOp dflags [res_q, res_r] WordQuotRem2Op [arg_x_high, arg_x_low, arg_y] _
    = do let ty = cmmExprType dflags arg_x_high
             shl   x i = CmmMachOp (MO_Shl   (wordWidth dflags)) [x, i]
             shr   x i = CmmMachOp (MO_U_Shr (wordWidth dflags)) [x, i]
             or    x y = CmmMachOp (MO_Or    (wordWidth dflags)) [x, y]
             ge    x y = CmmMachOp (MO_U_Ge  (wordWidth dflags)) [x, y]
             ne    x y = CmmMachOp (MO_Ne    (wordWidth dflags)) [x, y]
             minus x y = CmmMachOp (MO_Sub   (wordWidth dflags)) [x, y]
             times x y = CmmMachOp (MO_Mul   (wordWidth dflags)) [x, y]
             zero   = lit 0
             one    = lit 1
             negone = lit (fromIntegral (widthInBits (wordWidth dflags)) - 1)
             lit i = CmmLit (CmmInt i (wordWidth dflags))
             f :: Int -> CmmExpr -> CmmExpr -> CmmExpr -> FCode [CmmStmt]
             f 0 acc high _ = return [CmmAssign (CmmLocal res_q) acc,
                                      CmmAssign (CmmLocal res_r) high]
             f i acc high low =
                 do roverflowedBit <- newLocalReg ty
                    rhigh'         <- newLocalReg ty
                    rhigh''        <- newLocalReg ty
                    rlow'          <- newLocalReg ty
                    risge          <- newLocalReg ty
                    racc'          <- newLocalReg ty
                    let high'         = CmmReg (CmmLocal rhigh')
                        isge          = CmmReg (CmmLocal risge)
                        overflowedBit = CmmReg (CmmLocal roverflowedBit)
                    let this = [CmmAssign (CmmLocal roverflowedBit)
                                          (shr high negone),
                                CmmAssign (CmmLocal rhigh')
                                          (or (shl high one) (shr low negone)),
                                CmmAssign (CmmLocal rlow')
                                          (shl low one),
                                CmmAssign (CmmLocal risge)
                                          (or (overflowedBit `ne` zero)
                                              (high' `ge` arg_y)),
                                CmmAssign (CmmLocal rhigh'')
                                          (high' `minus` (arg_y `times` isge)),
                                CmmAssign (CmmLocal racc')
                                          (or (shl acc one) isge)]
                    rest <- f (i - 1) (CmmReg (CmmLocal racc'))
                                      (CmmReg (CmmLocal rhigh''))
                                      (CmmReg (CmmLocal rlow'))
                    return (this ++ rest)
         genericImpl <- f (widthInBits (wordWidth dflags)) zero arg_x_high arg_x_low
         let stmt = CmmCall (CmmPrim (MO_U_QuotRem2 (wordWidth dflags)) (Just genericImpl))
                            [CmmHinted res_q NoHint,
                             CmmHinted res_r NoHint]
                            [CmmHinted arg_x_high NoHint,
                             CmmHinted arg_x_low NoHint,
                             CmmHinted arg_y NoHint]
                            CmmMayReturn
         stmtC stmt

emitPrimOp dflags [res_h, res_l] WordAdd2Op [arg_x, arg_y] _
 = do r1 <- newLocalReg (cmmExprType dflags arg_x)
      r2 <- newLocalReg (cmmExprType dflags arg_x)
      -- This generic implementation is very simple and slow. We might
      -- well be able to do better, but for now this at least works.
      let genericImpl
           = [CmmAssign (CmmLocal r1)
                  (add (bottomHalf arg_x) (bottomHalf arg_y)),
              CmmAssign (CmmLocal r2)
                  (add (topHalf (CmmReg (CmmLocal r1)))
                       (add (topHalf arg_x) (topHalf arg_y))),
              CmmAssign (CmmLocal res_h)
                  (topHalf (CmmReg (CmmLocal r2))),
              CmmAssign (CmmLocal res_l)
                  (or (toTopHalf (CmmReg (CmmLocal r2)))
                      (bottomHalf (CmmReg (CmmLocal r1))))]
               where topHalf x = CmmMachOp (MO_U_Shr (wordWidth dflags)) [x, hww]
                     toTopHalf x = CmmMachOp (MO_Shl (wordWidth dflags)) [x, hww]
                     bottomHalf x = CmmMachOp (MO_And (wordWidth dflags)) [x, hwm]
                     add x y = CmmMachOp (MO_Add (wordWidth dflags)) [x, y]
                     or x y = CmmMachOp (MO_Or (wordWidth dflags)) [x, y]
                     hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth dflags)))
                                          (wordWidth dflags))
                     hwm = CmmLit (CmmInt (halfWordMask dflags) (wordWidth dflags))
          stmt = CmmCall (CmmPrim (MO_Add2 (wordWidth dflags)) (Just genericImpl))
                         [CmmHinted res_h NoHint,
                          CmmHinted res_l NoHint]
                         [CmmHinted arg_x NoHint,
                          CmmHinted arg_y NoHint]
                         CmmMayReturn
      stmtC stmt
emitPrimOp dflags [res_h, res_l] WordMul2Op [arg_x, arg_y] _
 = do let t = cmmExprType dflags arg_x
      xlyl <- liftM CmmLocal $ newLocalReg t
      xlyh <- liftM CmmLocal $ newLocalReg t
      xhyl <- liftM CmmLocal $ newLocalReg t
      r    <- liftM CmmLocal $ newLocalReg t
      -- This generic implementation is very simple and slow. We might
      -- well be able to do better, but for now this at least works.
      let genericImpl
           = [CmmAssign xlyl
                  (mul (bottomHalf arg_x) (bottomHalf arg_y)),
              CmmAssign xlyh
                  (mul (bottomHalf arg_x) (topHalf arg_y)),
              CmmAssign xhyl
                  (mul (topHalf arg_x) (bottomHalf arg_y)),
              CmmAssign r
                  (sum [topHalf    (CmmReg xlyl),
                        bottomHalf (CmmReg xhyl),
                        bottomHalf (CmmReg xlyh)]),
              CmmAssign (CmmLocal res_l)
                  (or (bottomHalf (CmmReg xlyl))
                      (toTopHalf (CmmReg r))),
              CmmAssign (CmmLocal res_h)
                  (sum [mul (topHalf arg_x) (topHalf arg_y),
                        topHalf (CmmReg xhyl),
                        topHalf (CmmReg xlyh),
                        topHalf (CmmReg r)])]
               where topHalf x = CmmMachOp (MO_U_Shr (wordWidth dflags)) [x, hww]
                     toTopHalf x = CmmMachOp (MO_Shl (wordWidth dflags)) [x, hww]
                     bottomHalf x = CmmMachOp (MO_And (wordWidth dflags)) [x, hwm]
                     add x y = CmmMachOp (MO_Add (wordWidth dflags)) [x, y]
                     sum = foldl1 add
                     mul x y = CmmMachOp (MO_Mul (wordWidth dflags)) [x, y]
                     or x y = CmmMachOp (MO_Or (wordWidth dflags)) [x, y]
                     hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth dflags)))
                                          (wordWidth dflags))
                     hwm = CmmLit (CmmInt (halfWordMask dflags) (wordWidth dflags))
          stmt = CmmCall (CmmPrim (MO_U_Mul2 (wordWidth dflags)) (Just genericImpl))
                         [CmmHinted res_h NoHint,
                          CmmHinted res_l NoHint]
                         [CmmHinted arg_x NoHint,
                          CmmHinted arg_y NoHint]
                         CmmMayReturn
      stmtC stmt

emitPrimOp _ _ op _ _
 = pprPanic "emitPrimOp: can't translate PrimOp" (ppr op)

newLocalReg :: CmmType -> FCode LocalReg
newLocalReg t = do u <- newUnique
                   return $ LocalReg u t

-- These PrimOps are NOPs in Cmm

nopOp :: PrimOp -> Bool
nopOp Int2WordOp     = True
nopOp Word2IntOp     = True
nopOp Int2AddrOp     = True
nopOp Addr2IntOp     = True
nopOp ChrOp          = True  -- Int# and Char# are rep'd the same
nopOp OrdOp          = True
nopOp _              = False

-- These PrimOps turn into double casts

narrowOp :: PrimOp -> Maybe (Width -> Width -> MachOp, Width)
narrowOp Narrow8IntOp   = Just (MO_SS_Conv, W8)
narrowOp Narrow16IntOp  = Just (MO_SS_Conv, W16)
narrowOp Narrow32IntOp  = Just (MO_SS_Conv, W32)
narrowOp Narrow8WordOp  = Just (MO_UU_Conv, W8)
narrowOp Narrow16WordOp = Just (MO_UU_Conv, W16)
narrowOp Narrow32WordOp = Just (MO_UU_Conv, W32)
narrowOp _              = Nothing

-- Native word signless ops

translateOp :: DynFlags -> PrimOp -> Maybe MachOp
translateOp dflags IntAddOp       = Just (mo_wordAdd dflags)
translateOp dflags IntSubOp       = Just (mo_wordSub dflags)
translateOp dflags WordAddOp      = Just (mo_wordAdd dflags)
translateOp dflags WordSubOp      = Just (mo_wordSub dflags)
translateOp dflags AddrAddOp      = Just (mo_wordAdd dflags)
translateOp dflags AddrSubOp      = Just (mo_wordSub dflags)

translateOp dflags IntEqOp        = Just (mo_wordEq dflags)
translateOp dflags IntNeOp        = Just (mo_wordNe dflags)
translateOp dflags WordEqOp       = Just (mo_wordEq dflags)
translateOp dflags WordNeOp       = Just (mo_wordNe dflags)
translateOp dflags AddrEqOp       = Just (mo_wordEq dflags)
translateOp dflags AddrNeOp       = Just (mo_wordNe dflags)

translateOp dflags AndOp          = Just (mo_wordAnd dflags)
translateOp dflags OrOp           = Just (mo_wordOr dflags)
translateOp dflags XorOp          = Just (mo_wordXor dflags)
translateOp dflags NotOp          = Just (mo_wordNot dflags)
translateOp dflags SllOp          = Just (mo_wordShl dflags)
translateOp dflags SrlOp          = Just (mo_wordUShr dflags)

translateOp dflags AddrRemOp      = Just (mo_wordURem dflags)

-- Native word signed ops

translateOp dflags IntMulOp        = Just (mo_wordMul dflags)
translateOp dflags IntMulMayOfloOp = Just (MO_S_MulMayOflo (wordWidth dflags))
translateOp dflags IntQuotOp       = Just (mo_wordSQuot dflags)
translateOp dflags IntRemOp        = Just (mo_wordSRem dflags)
translateOp dflags IntNegOp        = Just (mo_wordSNeg dflags)


translateOp dflags IntGeOp        = Just (mo_wordSGe dflags)
translateOp dflags IntLeOp        = Just (mo_wordSLe dflags)
translateOp dflags IntGtOp        = Just (mo_wordSGt dflags)
translateOp dflags IntLtOp        = Just (mo_wordSLt dflags)

translateOp dflags ISllOp         = Just (mo_wordShl dflags)
translateOp dflags ISraOp         = Just (mo_wordSShr dflags)
translateOp dflags ISrlOp         = Just (mo_wordUShr dflags)

-- Native word unsigned ops

translateOp dflags WordGeOp       = Just (mo_wordUGe dflags)
translateOp dflags WordLeOp       = Just (mo_wordULe dflags)
translateOp dflags WordGtOp       = Just (mo_wordUGt dflags)
translateOp dflags WordLtOp       = Just (mo_wordULt dflags)

translateOp dflags WordMulOp      = Just (mo_wordMul dflags)
translateOp dflags WordQuotOp     = Just (mo_wordUQuot dflags)
translateOp dflags WordRemOp      = Just (mo_wordURem dflags)

translateOp dflags AddrGeOp       = Just (mo_wordUGe dflags)
translateOp dflags AddrLeOp       = Just (mo_wordULe dflags)
translateOp dflags AddrGtOp       = Just (mo_wordUGt dflags)
translateOp dflags AddrLtOp       = Just (mo_wordULt dflags)

-- Char# ops

translateOp dflags CharEqOp       = Just (MO_Eq (wordWidth dflags))
translateOp dflags CharNeOp       = Just (MO_Ne (wordWidth dflags))
translateOp dflags CharGeOp       = Just (MO_U_Ge (wordWidth dflags))
translateOp dflags CharLeOp       = Just (MO_U_Le (wordWidth dflags))
translateOp dflags CharGtOp       = Just (MO_U_Gt (wordWidth dflags))
translateOp dflags CharLtOp       = Just (MO_U_Lt (wordWidth dflags))

-- Double ops

translateOp _      DoubleEqOp     = Just (MO_F_Eq W64)
translateOp _      DoubleNeOp     = Just (MO_F_Ne W64)
translateOp _      DoubleGeOp     = Just (MO_F_Ge W64)
translateOp _      DoubleLeOp     = Just (MO_F_Le W64)
translateOp _      DoubleGtOp     = Just (MO_F_Gt W64)
translateOp _      DoubleLtOp     = Just (MO_F_Lt W64)

translateOp _      DoubleAddOp    = Just (MO_F_Add W64)
translateOp _      DoubleSubOp    = Just (MO_F_Sub W64)
translateOp _      DoubleMulOp    = Just (MO_F_Mul W64)
translateOp _      DoubleDivOp    = Just (MO_F_Quot W64)
translateOp _      DoubleNegOp    = Just (MO_F_Neg W64)

-- Float ops

translateOp _      FloatEqOp     = Just (MO_F_Eq W32)
translateOp _      FloatNeOp     = Just (MO_F_Ne W32)
translateOp _      FloatGeOp     = Just (MO_F_Ge W32)
translateOp _      FloatLeOp     = Just (MO_F_Le W32)
translateOp _      FloatGtOp     = Just (MO_F_Gt W32)
translateOp _      FloatLtOp     = Just (MO_F_Lt W32)

translateOp _      FloatAddOp    = Just (MO_F_Add  W32)
translateOp _      FloatSubOp    = Just (MO_F_Sub  W32)
translateOp _      FloatMulOp    = Just (MO_F_Mul  W32)
translateOp _      FloatDivOp    = Just (MO_F_Quot W32)
translateOp _      FloatNegOp    = Just (MO_F_Neg  W32)

-- Conversions

translateOp dflags Int2DoubleOp   = Just (MO_SF_Conv (wordWidth dflags) W64)
translateOp dflags Double2IntOp   = Just (MO_FS_Conv W64 (wordWidth dflags))

translateOp dflags Int2FloatOp    = Just (MO_SF_Conv (wordWidth dflags) W32)
translateOp dflags Float2IntOp    = Just (MO_FS_Conv W32 (wordWidth dflags))

translateOp _      Float2DoubleOp = Just (MO_FF_Conv W32 W64)
translateOp _      Double2FloatOp = Just (MO_FF_Conv W64 W32)

-- Word comparisons masquerading as more exotic things.

translateOp dflags SameMutVarOp           = Just (mo_wordEq dflags)
translateOp dflags SameMVarOp             = Just (mo_wordEq dflags)
translateOp dflags SameMutableArrayOp     = Just (mo_wordEq dflags)
translateOp dflags SameMutableByteArrayOp = Just (mo_wordEq dflags)
translateOp dflags SameMutableArrayArrayOp= Just (mo_wordEq dflags)
translateOp dflags SameTVarOp             = Just (mo_wordEq dflags)
translateOp dflags EqStablePtrOp          = Just (mo_wordEq dflags)

translateOp _      _ = Nothing

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
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrWordsHdrSize dflags) maybe_post_read_cast rep res addr idx
doIndexByteArrayOp _ _ _ _
   = panic "CgPrimOp: doIndexByteArrayOp"

doReadPtrArrayOp :: LocalReg -> CmmExpr -> CmmExpr -> Code
doReadPtrArrayOp res addr idx
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrPtrsHdrSize dflags) Nothing (gcWord dflags) res addr idx


doWriteOffAddrOp, doWriteByteArrayOp
        :: Maybe MachOp -> CmmType
        -> [LocalReg] -> [CmmExpr] -> Code
doWriteOffAddrOp maybe_pre_write_cast rep [] [addr,idx,val]
   = mkBasicIndexedWrite 0 maybe_pre_write_cast rep addr idx val
doWriteOffAddrOp _ _ _ _
   = panic "CgPrimOp: doWriteOffAddrOp"

doWriteByteArrayOp maybe_pre_write_cast rep [] [addr,idx,val]
   = do dflags <- getDynFlags
        mkBasicIndexedWrite (arrWordsHdrSize dflags) maybe_pre_write_cast rep addr idx val
doWriteByteArrayOp _ _ _ _
   = panic "CgPrimOp: doWriteByteArrayOp"

doWritePtrArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> Code
doWritePtrArrayOp addr idx val
   = do dflags <- getDynFlags
        mkBasicIndexedWrite (arrPtrsHdrSize dflags) Nothing (bWord dflags) addr idx val
        stmtC (setInfo addr (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))
   -- the write barrier.  We must write a byte into the mark table:
   -- bits8[a + header_size + StgMutArrPtrs_size(a) + x >> N]
        stmtC $ CmmStore (
          cmmOffsetExpr dflags
           (cmmOffsetExprW dflags (cmmOffsetB dflags addr (arrPtrsHdrSize dflags))
                          (loadArrPtrsSize dflags addr))
           (card dflags idx)
          ) (CmmLit (CmmInt 1 W8))

loadArrPtrsSize :: DynFlags -> CmmExpr -> CmmExpr
loadArrPtrsSize dflags addr = CmmLoad (cmmOffsetB dflags addr off) (bWord dflags)
 where off = fixedHdrSize dflags * wORD_SIZE dflags + oFFSET_StgMutArrPtrs_ptrs dflags

mkBasicIndexedRead :: ByteOff -> Maybe MachOp -> CmmType
                   -> LocalReg -> CmmExpr -> CmmExpr -> Code
mkBasicIndexedRead off Nothing read_rep res base idx
   = do dflags <- getDynFlags
        stmtC (CmmAssign (CmmLocal res) (cmmLoadIndexOffExpr dflags off read_rep base idx))
mkBasicIndexedRead off (Just cast) read_rep res base idx
   = do dflags <- getDynFlags
        stmtC (CmmAssign (CmmLocal res) (CmmMachOp cast [
                                cmmLoadIndexOffExpr dflags off read_rep base idx]))

mkBasicIndexedWrite :: ByteOff -> Maybe MachOp -> CmmType
                    -> CmmExpr -> CmmExpr -> CmmExpr -> Code
mkBasicIndexedWrite off Nothing write_rep base idx val
   = do dflags <- getDynFlags
        stmtC (CmmStore (cmmIndexOffExpr dflags off write_rep base idx) val)
mkBasicIndexedWrite off (Just cast) write_rep base idx val
   = do dflags <- getDynFlags
        stmtC (CmmStore (cmmIndexOffExpr dflags off write_rep base idx) (CmmMachOp cast [val]))

-- ----------------------------------------------------------------------------
-- Misc utils

cmmIndexOffExpr :: DynFlags -> ByteOff -> CmmType -> CmmExpr -> CmmExpr -> CmmExpr
cmmIndexOffExpr dflags off rep base idx
   = cmmIndexExpr dflags (typeWidth rep) (cmmOffsetB dflags base off) idx

cmmLoadIndexOffExpr :: DynFlags -> ByteOff -> CmmType -> CmmExpr -> CmmExpr -> CmmExpr
cmmLoadIndexOffExpr dflags off rep base idx
   = CmmLoad (cmmIndexOffExpr dflags off rep base idx) rep

setInfo :: CmmExpr -> CmmExpr -> CmmStmt
setInfo closure_ptr info_ptr = CmmStore closure_ptr info_ptr

-- ----------------------------------------------------------------------------
-- Copying byte arrays

-- | Takes a source 'ByteArray#', an offset in the source array, a
-- destination 'MutableByteArray#', an offset into the destination
-- array, and the number of bytes to copy.  Copies the given number of
-- bytes from the source array to the destination array.
doCopyByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> StgLiveVars -> Code
doCopyByteArrayOp = emitCopyByteArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes live =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p bytes (CmmLit (mkIntCLit dflags 1)) live

-- | Takes a source 'MutableByteArray#', an offset in the source
-- array, a destination 'MutableByteArray#', an offset into the
-- destination array, and the number of bytes to copy.  Copies the
-- given number of bytes from the source array to the destination
-- array.
doCopyMutableByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                         -> StgLiveVars -> Code
doCopyMutableByteArrayOp = emitCopyByteArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes live =
        do dflags <- getDynFlags
           emitIfThenElse (cmmEqWord dflags src dst)
               (emitMemmoveCall dst_p src_p bytes (CmmLit (mkIntCLit dflags 1)) live)
               (emitMemcpyCall dst_p src_p bytes (CmmLit (mkIntCLit dflags 1)) live)

emitCopyByteArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> StgLiveVars -> Code)
                  -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> StgLiveVars
                  -> Code
emitCopyByteArray copy src src_off dst dst_off n live = do
    dflags <- getDynFlags
    dst_p <- assignTemp $ cmmOffsetExpr dflags (cmmOffsetB dflags dst (arrWordsHdrSize dflags)) dst_off
    src_p <- assignTemp $ cmmOffsetExpr dflags (cmmOffsetB dflags src (arrWordsHdrSize dflags)) src_off
    copy src dst dst_p src_p n live

-- ----------------------------------------------------------------------------
-- Setting byte arrays

-- | Takes a 'MutableByteArray#', an offset into the array, a length,
-- and a byte, and sets each of the selected bytes in the array to the
-- character.
doSetByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                 -> StgLiveVars -> Code
doSetByteArrayOp ba off len c live
    = do dflags <- getDynFlags
         p <- assignTemp $ cmmOffsetExpr dflags (cmmOffsetB dflags ba (arrWordsHdrSize dflags)) off
         emitMemsetCall p c len (CmmLit (mkIntCLit dflags 1)) live

-- ----------------------------------------------------------------------------
-- Copying pointer arrays

-- EZY: This code has an unusually high amount of assignTemp calls, seen
-- nowhere else in the code generator.  This is mostly because these
-- "primitive" ops result in a surprisingly large amount of code.  It
-- will likely be worthwhile to optimize what is emitted here, so that
-- our optimization passes don't waste time repeatedly optimizing the
-- same bits of code.

-- | Takes a source 'Array#', an offset in the source array, a
-- destination 'MutableArray#', an offset into the destination array,
-- and the number of elements to copy.  Copies the given number of
-- elements from the source array to the destination array.
doCopyArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
              -> StgLiveVars -> Code
doCopyArrayOp = emitCopyArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes live =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p bytes (CmmLit (mkIntCLit dflags (wORD_SIZE dflags))) live

-- | Takes a source 'MutableArray#', an offset in the source array, a
-- destination 'MutableArray#', an offset into the destination array,
-- and the number of elements to copy.  Copies the given number of
-- elements from the source array to the destination array.
doCopyMutableArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                     -> StgLiveVars -> Code
doCopyMutableArrayOp = emitCopyArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes live =
        do dflags <- getDynFlags
           emitIfThenElse (cmmEqWord dflags src dst)
               (emitMemmoveCall dst_p src_p bytes (CmmLit (mkIntCLit dflags (wORD_SIZE dflags))) live)
               (emitMemcpyCall dst_p src_p bytes (CmmLit (mkIntCLit dflags (wORD_SIZE dflags))) live)

emitCopyArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> StgLiveVars -> Code)
              -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
              -> StgLiveVars
              -> Code
emitCopyArray copy src0 src_off0 dst0 dst_off0 n0 live = do
    dflags <- getDynFlags
    -- Assign the arguments to temporaries so the code generator can
    -- calculate liveness for us.
    n <- assignTemp_ n0
    emitIf (cmmNeWord dflags n (CmmLit (mkIntCLit dflags 0))) $ do
        src <- assignTemp_ src0
        src_off <- assignTemp_ src_off0
        dst <- assignTemp_ dst0
        dst_off <- assignTemp_ dst_off0

        -- Set the dirty bit in the header.
        stmtC (setInfo dst (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))

        dst_elems_p <- assignTemp $ cmmOffsetB dflags dst (arrPtrsHdrSize dflags)
        dst_p <- assignTemp $ cmmOffsetExprW dflags dst_elems_p dst_off
        src_p <- assignTemp $ cmmOffsetExprW dflags (cmmOffsetB dflags src (arrPtrsHdrSize dflags)) src_off
        bytes <- assignTemp $ cmmMulWord dflags n (CmmLit (mkIntCLit dflags (wORD_SIZE dflags)))

        copy src dst dst_p src_p bytes live

        -- The base address of the destination card table
        dst_cards_p <- assignTemp $ cmmOffsetExprW dflags dst_elems_p (loadArrPtrsSize dflags dst)

        emitSetCards dst_off dst_cards_p n live

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy.  Allocates a new array and
-- initializes it form the source array.
emitCloneArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> CmmExpr
               -> StgLiveVars -> Code
emitCloneArray info_p res_r src0 src_off0 n0 live = do
    dflags <- getDynFlags
    let arrPtrsHdrSizeW dflags = CmmLit $ mkIntCLit dflags $ fixedHdrSize dflags +
                                     (sIZEOF_StgMutArrPtrs_NoHdr dflags `div` wORD_SIZE dflags)
        myCapability = cmmSubWord dflags (CmmReg baseReg)
                                         (CmmLit (mkIntCLit dflags (oFFSET_Capability_r dflags)))
    -- Assign the arguments to temporaries so the code generator can
    -- calculate liveness for us.
    src <- assignTemp_ src0
    src_off <- assignTemp_ src_off0
    n <- assignTemp_ n0

    card_bytes <- assignTemp $ cardRoundUp dflags n
    size <- assignTemp $ cmmAddWord dflags n (bytesToWordsRoundUp dflags card_bytes)
    words <- assignTemp $ cmmAddWord dflags (arrPtrsHdrSizeW dflags) size

    arr_r <- newTemp (bWord dflags)
    emitAllocateCall arr_r myCapability words live
    tickyAllocPrim (CmmLit (mkIntCLit dflags (arrPtrsHdrSize dflags))) (cmmMulWord dflags n (wordSize dflags))
        (CmmLit $ mkIntCLit dflags 0)

    let arr = CmmReg (CmmLocal arr_r)
    emitSetDynHdr arr (CmmLit (CmmLabel info_p)) curCCS
    stmtC $ CmmStore (cmmOffsetB dflags arr (fixedHdrSize dflags * wORD_SIZE dflags +
                                             oFFSET_StgMutArrPtrs_ptrs dflags)) n
    stmtC $ CmmStore (cmmOffsetB dflags arr (fixedHdrSize dflags * wORD_SIZE dflags +
                                             oFFSET_StgMutArrPtrs_size dflags)) size

    dst_p <- assignTemp $ cmmOffsetB dflags arr (arrPtrsHdrSize dflags)
    src_p <- assignTemp $ cmmOffsetExprW dflags (cmmOffsetB dflags src (arrPtrsHdrSize dflags))
             src_off

    emitMemcpyCall dst_p src_p (cmmMulWord dflags n (wordSize dflags))
        (CmmLit (mkIntCLit dflags (wORD_SIZE dflags))) live

    emitMemsetCall (cmmOffsetExprW dflags dst_p n)
        (CmmLit (mkIntCLit dflags 1))
        card_bytes
        (CmmLit (mkIntCLit dflags (wORD_SIZE dflags)))
        live
    stmtC $ CmmAssign (CmmLocal res_r) arr

-- | Takes and offset in the destination array, the base address of
-- the card table, and the number of elements affected (*not* the
-- number of cards). The number of elements may not be zero.
-- Marks the relevant cards as dirty.
emitSetCards :: CmmExpr -> CmmExpr -> CmmExpr -> StgLiveVars -> Code
emitSetCards dst_start dst_cards_start n live = do
    dflags <- getDynFlags
    start_card <- assignTemp $ card dflags dst_start
    let end_card = card dflags (cmmAddWord dflags dst_start n)
    emitMemsetCall (cmmAddWord dflags dst_cards_start start_card)
        (CmmLit (mkIntCLit dflags 1))
        (cmmAddWord dflags (cmmSubWord dflags end_card start_card) (CmmLit (mkIntCLit dflags 1)))
        (CmmLit (mkIntCLit dflags 1)) -- no alignment (1 byte)
        live

-- Convert an element index to a card index
card :: DynFlags -> CmmExpr -> CmmExpr
card dflags i = cmmUShrWord dflags i (CmmLit (mkIntCLit dflags (mUT_ARR_PTRS_CARD_BITS dflags)))

-- Convert a number of elements to a number of cards, rounding up
cardRoundUp :: DynFlags -> CmmExpr -> CmmExpr
cardRoundUp dflags i = card dflags (cmmAddWord dflags i (CmmLit (mkIntCLit dflags ((1 `shiftL` mUT_ARR_PTRS_CARD_BITS dflags) - 1))))

bytesToWordsRoundUp :: DynFlags -> CmmExpr -> CmmExpr
bytesToWordsRoundUp dflags e
    = cmmQuotWord dflags
          (cmmAddWord dflags e (CmmLit (mkIntCLit dflags (wORD_SIZE dflags - 1))))
          (wordSize dflags)

wordSize :: DynFlags -> CmmExpr
wordSize dflags = CmmLit (mkIntCLit dflags (wORD_SIZE dflags))

-- | Emit a call to @memcpy@.
emitMemcpyCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> StgLiveVars
               -> Code
emitMemcpyCall dst src n align live = do
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
        [{-no results-}]
        (CmmPrim MO_Memcpy Nothing)
        [ (CmmHinted dst AddrHint)
        , (CmmHinted src AddrHint)
        , (CmmHinted n NoHint)
        , (CmmHinted align NoHint)
        ]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn

-- | Emit a call to @memmove@.
emitMemmoveCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> StgLiveVars
                -> Code
emitMemmoveCall dst src n align live = do
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
        [{-no results-}]
        (CmmPrim MO_Memmove Nothing)
        [ (CmmHinted dst AddrHint)
        , (CmmHinted src AddrHint)
        , (CmmHinted n NoHint)
        , (CmmHinted align NoHint)
        ]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn

-- | Emit a call to @memset@.  The second argument must be a word but
-- its value must fit inside an unsigned char.
emitMemsetCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> StgLiveVars
               -> Code
emitMemsetCall dst c n align live = do
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
        [{-no results-}]
        (CmmPrim MO_Memset Nothing)
        [ (CmmHinted dst AddrHint)
        , (CmmHinted c NoHint)
        , (CmmHinted n NoHint)
        , (CmmHinted align NoHint)
        ]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn

-- | Emit a call to @allocate@.
emitAllocateCall :: LocalReg -> CmmExpr -> CmmExpr -> StgLiveVars -> Code
emitAllocateCall res cap n live = do
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
        [CmmHinted res AddrHint]
        (CmmCallee allocate CCallConv)
        [ (CmmHinted cap AddrHint)
        , (CmmHinted n NoHint)
        ]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn
  where
    allocate = CmmLit (CmmLabel (mkForeignLabel (fsLit "allocate") Nothing
                                 ForeignLabelInExternalPackage IsFunction))

emitPopCntCall :: LocalReg -> CmmExpr -> Width -> StgLiveVars -> Code
emitPopCntCall res x width live = do
    vols <- getVolatileRegs live
    emitForeignCall' PlayRisky
        [CmmHinted res NoHint]
        (CmmPrim (MO_PopCnt width) Nothing)
        [(CmmHinted x NoHint)]
        (Just vols)
        NoC_SRT -- No SRT b/c we do PlayRisky
        CmmMayReturn
