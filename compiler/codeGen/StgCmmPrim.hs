-----------------------------------------------------------------------------
--
-- Stg to C--: primitive operations
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmPrim (
   cgOpApp,
   cgPrimOp -- internal(ish), used by cgCase to get code for a
            -- comparison without also turning it into a Bool.
 ) where

#include "HsVersions.h"

import StgCmmLayout
import StgCmmForeign
import StgCmmEnv
import StgCmmMonad
import StgCmmUtils
import StgCmmTicky
import StgCmmHeap
import StgCmmProf

import DynFlags
import Platform
import BasicTypes
import MkGraph
import StgSyn
import Cmm
import CmmInfo
import Type     ( Type, tyConAppTyCon )
import TyCon
import CLabel
import CmmUtils
import PrimOp
import SMRep
import Module
import FastString
import Outputable
import Util

import Control.Monad (liftM)
import Data.Bits

------------------------------------------------------------------------
--      Primitive operations and foreign calls
------------------------------------------------------------------------

{- Note [Foreign call results]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
A foreign call always returns an unboxed tuple of results, one
of which is the state token.  This seems to happen even for pure
calls.

Even if we returned a single result for pure calls, it'd still be
right to wrap it in a singleton unboxed tuple, because the result
might be a Haskell closure pointer, we don't want to evaluate it. -}

----------------------------------
cgOpApp :: StgOp        -- The op
        -> [StgArg]     -- Arguments
        -> Type         -- Result type (always an unboxed tuple)
        -> FCode ReturnKind

-- Foreign calls
cgOpApp (StgFCallOp fcall _) stg_args res_ty
  = cgForeignCall fcall stg_args res_ty
      -- Note [Foreign call results]

-- tagToEnum# is special: we need to pull the constructor
-- out of the table, and perform an appropriate return.

cgOpApp (StgPrimOp TagToEnumOp) [arg] res_ty
  = ASSERT(isEnumerationTyCon tycon)
    do  { dflags <- getDynFlags
        ; args' <- getNonVoidArgAmodes [arg]
        ; let amode = case args' of [amode] -> amode
                                    _ -> panic "TagToEnumOp had void arg"
        ; emitReturn [tagToClosure dflags tycon amode] }
   where
          -- If you're reading this code in the attempt to figure
          -- out why the compiler panic'ed here, it is probably because
          -- you used tagToEnum# in a non-monomorphic setting, e.g.,
          --         intToTg :: Enum a => Int -> a ; intToTg (I# x#) = tagToEnum# x#
          -- That won't work.
        tycon = tyConAppTyCon res_ty

cgOpApp (StgPrimOp primop) args res_ty
  | primOpOutOfLine primop
  = do  { cmm_args <- getNonVoidArgAmodes args
        ; let fun = CmmLit (CmmLabel (mkRtsPrimOpLabel primop))
        ; emitCall (NativeNodeCall, NativeReturn) fun cmm_args }

  | ReturnsPrim VoidRep <- result_info
  = do cgPrimOp [] primop args
       emitReturn []

  | ReturnsPrim rep <- result_info
  = do dflags <- getDynFlags
       res <- newTemp (primRepCmmType dflags rep)
       cgPrimOp [res] primop args
       emitReturn [CmmReg (CmmLocal res)]

  | ReturnsAlg tycon <- result_info, isUnboxedTupleTyCon tycon
  = do (regs, _hints) <- newUnboxedTupleRegs res_ty
       cgPrimOp regs primop args
       emitReturn (map (CmmReg . CmmLocal) regs)

  | ReturnsAlg tycon <- result_info
  , isEnumerationTyCon tycon
        -- c.f. cgExpr (...TagToEnumOp...)
  = do dflags <- getDynFlags
       tag_reg <- newTemp (bWord dflags)
       cgPrimOp [tag_reg] primop args
       emitReturn [tagToClosure dflags tycon
                                (CmmReg (CmmLocal tag_reg))]

  | otherwise = panic "cgPrimop"
  where
     result_info = getPrimOpResultInfo primop

cgOpApp (StgPrimCallOp primcall) args _res_ty
  = do  { cmm_args <- getNonVoidArgAmodes args
        ; let fun = CmmLit (CmmLabel (mkPrimCallLabel primcall))
        ; emitCall (NativeNodeCall, NativeReturn) fun cmm_args }

---------------------------------------------------
cgPrimOp   :: [LocalReg]        -- where to put the results
           -> PrimOp            -- the op
           -> [StgArg]          -- arguments
           -> FCode ()

cgPrimOp results op args
  = do dflags <- getDynFlags
       arg_exprs <- getNonVoidArgAmodes args
       emitPrimOp dflags results op arg_exprs


------------------------------------------------------------------------
--      Emitting code for a primop
------------------------------------------------------------------------

emitPrimOp :: DynFlags
           -> [LocalReg]        -- where to put the results
           -> PrimOp            -- the op
           -> [CmmExpr]         -- arguments
           -> FCode ()

-- First we handle various awkward cases specially.  The remaining
-- easy cases are then handled by translateOp, defined below.

emitPrimOp dflags [res_r,res_c] IntAddCOp [aa,bb]
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
   = emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordAdd dflags) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr dflags) [
                CmmMachOp (mo_wordAnd dflags) [
                    CmmMachOp (mo_wordNot dflags) [CmmMachOp (mo_wordXor dflags) [aa,bb]],
                    CmmMachOp (mo_wordXor dflags) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr dflags (wORD_SIZE_IN_BITS dflags - 1)
          ]
     ]


emitPrimOp dflags [res_r,res_c] IntSubCOp [aa,bb]
{- Similarly:
   #define subIntCzh(r,c,a,b)                                   \
   { r = ((I_)(a)) - ((I_)(b));                                 \
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))     \
         >> (BITS_IN (I_) - 1);                                 \
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)
-}
   = emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordSub dflags) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr dflags) [
                CmmMachOp (mo_wordAnd dflags) [
                    CmmMachOp (mo_wordXor dflags) [aa,bb],
                    CmmMachOp (mo_wordXor dflags) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr dflags (wORD_SIZE_IN_BITS dflags - 1)
          ]
     ]


emitPrimOp _ [res] ParOp [arg]
  =
        -- for now, just implement this in a C function
        -- later, we might want to inline it.
    emitCCall
        [(res,NoHint)]
        (CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "newSpark"))))
        [(CmmReg (CmmGlobal BaseReg), AddrHint), (arg,AddrHint)]

emitPrimOp dflags [res] SparkOp [arg]
  = do
        -- returns the value of arg in res.  We're going to therefore
        -- refer to arg twice (once to pass to newSpark(), and once to
        -- assign to res), so put it in a temporary.
        tmp <- assignTemp arg
        tmp2 <- newTemp (bWord dflags)
        emitCCall
            [(tmp2,NoHint)]
            (CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "newSpark"))))
            [(CmmReg (CmmGlobal BaseReg), AddrHint), ((CmmReg (CmmLocal tmp)), AddrHint)]
        emitAssign (CmmLocal res) (CmmReg (CmmLocal tmp))

emitPrimOp dflags [res] GetCCSOfOp [arg]
  = emitAssign (CmmLocal res) val
  where
    val
     | gopt Opt_SccProfilingOn dflags = costCentreFrom dflags (cmmUntag dflags arg)
     | otherwise                      = CmmLit (zeroCLit dflags)

emitPrimOp _ [res] GetCurrentCCSOp [_dummy_arg]
   = emitAssign (CmmLocal res) curCCS

emitPrimOp dflags [res] ReadMutVarOp [mutv]
   = emitAssign (CmmLocal res) (cmmLoadIndexW dflags mutv (fixedHdrSize dflags) (gcWord dflags))

emitPrimOp dflags [] WriteMutVarOp [mutv,var]
   = do emitStore (cmmOffsetW dflags mutv (fixedHdrSize dflags)) var
        emitCCall
                [{-no results-}]
                (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
                [(CmmReg (CmmGlobal BaseReg), AddrHint), (mutv,AddrHint)]

--  #define sizzeofByteArrayzh(r,a) \
--     r = ((StgArrWords *)(a))->bytes
emitPrimOp dflags [res] SizeofByteArrayOp [arg]
   = emit $ mkAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSize dflags) (bWord dflags))

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrWords *)(a))->bytes
emitPrimOp dflags [res] SizeofMutableByteArrayOp [arg]
   = emitPrimOp dflags [res] SizeofByteArrayOp [arg]


--  #define touchzh(o)                  /* nothing */
emitPrimOp _ res@[] TouchOp args@[_arg]
   = do emitPrimCall res MO_Touch args

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
emitPrimOp dflags [res] ByteArrayContents_Char [arg]
   = emitAssign (CmmLocal res) (cmmOffsetB dflags arg (arrWordsHdrSize dflags))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
emitPrimOp dflags [res] StableNameToIntOp [arg]
   = emitAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSize dflags) (bWord dflags))

--  #define eqStableNamezh(r,sn1,sn2)                                   \
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
emitPrimOp dflags [res] EqStableNameOp [arg1,arg2]
   = emitAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [
                                   cmmLoadIndexW dflags arg1 (fixedHdrSize dflags) (bWord dflags),
                                   cmmLoadIndexW dflags arg2 (fixedHdrSize dflags) (bWord dflags)
                         ])


emitPrimOp dflags [res] ReallyUnsafePtrEqualityOp [arg1,arg2]
   = emitAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [arg1,arg2])

--  #define addrToHValuezh(r,a) r=(P_)a
emitPrimOp _      [res] AddrToAnyOp [arg]
   = emitAssign (CmmLocal res) arg

--  #define dataToTagzh(r,a)  r=(GET_TAG(((StgClosure *)a)->header.info))
--  Note: argument may be tagged!
emitPrimOp dflags [res] DataToTagOp [arg]
   = emitAssign (CmmLocal res) (getConstrTag dflags (cmmUntag dflags arg))

{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.  -}

--  #define unsafeFreezzeArrayzh(r,a)
--      {
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN0_info);
--        r = a;
--      }
emitPrimOp _      [res] UnsafeFreezeArrayOp [arg]
   = emit $ catAGraphs
   [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
     mkAssign (CmmLocal res) arg ]
emitPrimOp _      [res] UnsafeFreezeArrayArrayOp [arg]
   = emit $ catAGraphs
   [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_infoLabel)),
     mkAssign (CmmLocal res) arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)       r=(a)
emitPrimOp _      [res] UnsafeFreezeByteArrayOp [arg]
   = emitAssign (CmmLocal res) arg

-- Copying pointer arrays

emitPrimOp _      [] CopyArrayOp [src,src_off,dst,dst_off,n] =
    doCopyArrayOp src src_off dst dst_off n
emitPrimOp _      [] CopyMutableArrayOp [src,src_off,dst,dst_off,n] =
    doCopyMutableArrayOp src src_off dst dst_off n
emitPrimOp _      [res] CloneArrayOp [src,src_off,n] =
    emitCloneArray mkMAP_FROZEN_infoLabel res src src_off n
emitPrimOp _      [res] CloneMutableArrayOp [src,src_off,n] =
    emitCloneArray mkMAP_DIRTY_infoLabel res src src_off n
emitPrimOp _      [res] FreezeArrayOp [src,src_off,n] =
    emitCloneArray mkMAP_FROZEN_infoLabel res src src_off n
emitPrimOp _      [res] ThawArrayOp [src,src_off,n] =
    emitCloneArray mkMAP_DIRTY_infoLabel res src src_off n

emitPrimOp _      [] CopyArrayArrayOp [src,src_off,dst,dst_off,n] =
    doCopyArrayOp src src_off dst dst_off n
emitPrimOp _      [] CopyMutableArrayArrayOp [src,src_off,dst,dst_off,n] =
    doCopyMutableArrayOp src src_off dst dst_off n

-- Reading/writing pointer arrays

emitPrimOp _      [res] ReadArrayOp  [obj,ix]    = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] IndexArrayOp [obj,ix]    = doReadPtrArrayOp res obj ix
emitPrimOp _      []  WriteArrayOp [obj,ix,v]  = doWritePtrArrayOp obj ix v

emitPrimOp _      [res] IndexArrayArrayOp_ByteArray         [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] IndexArrayArrayOp_ArrayArray        [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] ReadArrayArrayOp_ByteArray          [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] ReadArrayArrayOp_MutableByteArray   [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] ReadArrayArrayOp_ArrayArray         [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      [res] ReadArrayArrayOp_MutableArrayArray  [obj,ix]   = doReadPtrArrayOp res obj ix
emitPrimOp _      []  WriteArrayArrayOp_ByteArray         [obj,ix,v] = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_MutableByteArray  [obj,ix,v] = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_ArrayArray        [obj,ix,v] = doWritePtrArrayOp obj ix v
emitPrimOp _      []  WriteArrayArrayOp_MutableArrayArray [obj,ix,v] = doWritePtrArrayOp obj ix v

emitPrimOp dflags [res] SizeofArrayOp [arg]
   = emit $ mkAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags) (bWord dflags))
emitPrimOp dflags [res] SizeofMutableArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]
emitPrimOp dflags [res] SizeofArrayArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]
emitPrimOp dflags [res] SizeofMutableArrayArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]

-- IndexXXXoffAddr

emitPrimOp dflags res IndexOffAddrOp_Char             args = doIndexOffAddrOp   (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res IndexOffAddrOp_WideChar         args = doIndexOffAddrOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res IndexOffAddrOp_Int              args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Word             args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Addr             args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp _      res IndexOffAddrOp_Float            args = doIndexOffAddrOp   Nothing f32 res args
emitPrimOp _      res IndexOffAddrOp_Double           args = doIndexOffAddrOp   Nothing f64 res args
emitPrimOp dflags res IndexOffAddrOp_StablePtr        args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexOffAddrOp_Int8             args = doIndexOffAddrOp   (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexOffAddrOp_Int16            args = doIndexOffAddrOp   (Just (mo_s_16ToWord dflags)) b16 res args
emitPrimOp dflags res IndexOffAddrOp_Int32            args = doIndexOffAddrOp   (Just (mo_s_32ToWord dflags)) b32 res args
emitPrimOp _      res IndexOffAddrOp_Int64            args = doIndexOffAddrOp   Nothing b64 res args
emitPrimOp dflags res IndexOffAddrOp_Word8            args = doIndexOffAddrOp   (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexOffAddrOp_Word16           args = doIndexOffAddrOp   (Just (mo_u_16ToWord dflags)) b16 res args
emitPrimOp dflags res IndexOffAddrOp_Word32           args = doIndexOffAddrOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp _      res IndexOffAddrOp_Word64           args = doIndexOffAddrOp   Nothing b64 res args
emitPrimOp _      res IndexOffAddrOp_FloatX4          args = doIndexOffAddrOp   Nothing vec4f32 res args
emitPrimOp _      res IndexOffAddrOp_FloatAsFloatX4   args = doIndexOffAddrOpAs Nothing vec4f32 f32 res args
emitPrimOp _      res IndexOffAddrOp_DoubleX2         args = doIndexOffAddrOp   Nothing vec2f64 res args
emitPrimOp _      res IndexOffAddrOp_DoubleAsDoubleX2 args = doIndexOffAddrOpAs Nothing vec2f64 f64 res args
emitPrimOp _      res IndexOffAddrOp_Int32X4          args = doIndexOffAddrOp   Nothing vec4b32 res args
emitPrimOp _      res IndexOffAddrOp_Int32AsInt32X4   args = doIndexOffAddrOpAs Nothing vec4b32 b32 res args
emitPrimOp _      res IndexOffAddrOp_Int64X2          args = doIndexOffAddrOp   Nothing vec2b64 res args
emitPrimOp _      res IndexOffAddrOp_Int64AsInt64X2   args = doIndexOffAddrOpAs Nothing vec2b64 b64 res args

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

emitPrimOp dflags res ReadOffAddrOp_Char             args = doIndexOffAddrOp   (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res ReadOffAddrOp_WideChar         args = doIndexOffAddrOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res ReadOffAddrOp_Int              args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Word             args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Addr             args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp _      res ReadOffAddrOp_Float            args = doIndexOffAddrOp   Nothing f32 res args
emitPrimOp _      res ReadOffAddrOp_Double           args = doIndexOffAddrOp   Nothing f64 res args
emitPrimOp dflags res ReadOffAddrOp_StablePtr        args = doIndexOffAddrOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadOffAddrOp_Int8             args = doIndexOffAddrOp   (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadOffAddrOp_Int16            args = doIndexOffAddrOp   (Just (mo_s_16ToWord dflags)) b16 res args
emitPrimOp dflags res ReadOffAddrOp_Int32            args = doIndexOffAddrOp   (Just (mo_s_32ToWord dflags)) b32 res args
emitPrimOp _      res ReadOffAddrOp_Int64            args = doIndexOffAddrOp   Nothing b64 res args
emitPrimOp dflags res ReadOffAddrOp_Word8            args = doIndexOffAddrOp   (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadOffAddrOp_Word16           args = doIndexOffAddrOp   (Just (mo_u_16ToWord dflags)) b16 res args
emitPrimOp dflags res ReadOffAddrOp_Word32           args = doIndexOffAddrOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp _      res ReadOffAddrOp_Word64           args = doIndexOffAddrOp   Nothing b64 res args
emitPrimOp _      res ReadOffAddrOp_FloatX4          args = doIndexOffAddrOp   Nothing vec4f32 res args
emitPrimOp _      res ReadOffAddrOp_FloatAsFloatX4   args = doIndexOffAddrOpAs Nothing vec4f32 b32 res args
emitPrimOp _      res ReadOffAddrOp_DoubleX2         args = doIndexOffAddrOp   Nothing vec2f64 res args
emitPrimOp _      res ReadOffAddrOp_DoubleAsDoubleX2 args = doIndexOffAddrOpAs Nothing vec2f64 b64 res args
emitPrimOp _      res ReadOffAddrOp_Int32X4          args = doIndexOffAddrOp   Nothing vec4b32 res args
emitPrimOp _      res ReadOffAddrOp_Int32AsInt32X4   args = doIndexOffAddrOpAs Nothing vec4b32 b32 res args
emitPrimOp _      res ReadOffAddrOp_Int64X2          args = doIndexOffAddrOp   Nothing vec2b64 res args
emitPrimOp _      res ReadOffAddrOp_Int64AsInt64X2   args = doIndexOffAddrOpAs Nothing vec2b64 b64 res args

-- IndexXXXArray

emitPrimOp dflags res IndexByteArrayOp_Char             args = doIndexByteArrayOp   (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res IndexByteArrayOp_WideChar         args = doIndexByteArrayOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res IndexByteArrayOp_Int              args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Word             args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Addr             args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp _      res IndexByteArrayOp_Float            args = doIndexByteArrayOp   Nothing f32 res args
emitPrimOp _      res IndexByteArrayOp_Double           args = doIndexByteArrayOp   Nothing f64 res args
emitPrimOp dflags res IndexByteArrayOp_StablePtr        args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res IndexByteArrayOp_Int8             args = doIndexByteArrayOp   (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexByteArrayOp_Int16            args = doIndexByteArrayOp   (Just (mo_s_16ToWord dflags)) b16  res args
emitPrimOp dflags res IndexByteArrayOp_Int32            args = doIndexByteArrayOp   (Just (mo_s_32ToWord dflags)) b32  res args
emitPrimOp _      res IndexByteArrayOp_Int64            args = doIndexByteArrayOp   Nothing b64  res args
emitPrimOp dflags res IndexByteArrayOp_Word8            args = doIndexByteArrayOp   (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res IndexByteArrayOp_Word16           args = doIndexByteArrayOp   (Just (mo_u_16ToWord dflags)) b16  res args
emitPrimOp dflags res IndexByteArrayOp_Word32           args = doIndexByteArrayOp   (Just (mo_u_32ToWord dflags)) b32  res args
emitPrimOp _      res IndexByteArrayOp_Word64           args = doIndexByteArrayOp   Nothing b64  res args
emitPrimOp _      res IndexByteArrayOp_FloatX4          args = doIndexByteArrayOp   Nothing vec4f32 res args
emitPrimOp _      res IndexByteArrayOp_FloatAsFloatX4   args = doIndexByteArrayOpAs Nothing vec4f32 f32 res args
emitPrimOp _      res IndexByteArrayOp_DoubleX2         args = doIndexByteArrayOp   Nothing vec2f64 res args
emitPrimOp _      res IndexByteArrayOp_DoubleAsDoubleX2 args = doIndexByteArrayOpAs Nothing vec2f64 f64 res args
emitPrimOp _      res IndexByteArrayOp_Int32X4          args = doIndexByteArrayOp   Nothing vec4b32 res args
emitPrimOp _      res IndexByteArrayOp_Int32AsInt32X4   args = doIndexByteArrayOpAs Nothing vec4b32 b32 res args
emitPrimOp _      res IndexByteArrayOp_Int64X2          args = doIndexByteArrayOp   Nothing vec2b64 res args
emitPrimOp _      res IndexByteArrayOp_Int64AsInt64X2   args = doIndexByteArrayOpAs Nothing vec2b64 b64 res args

-- ReadXXXArray, identical to IndexXXXArray.

emitPrimOp dflags res ReadByteArrayOp_Char             args = doIndexByteArrayOp   (Just (mo_u_8ToWord dflags)) b8 res args
emitPrimOp dflags res ReadByteArrayOp_WideChar         args = doIndexByteArrayOp   (Just (mo_u_32ToWord dflags)) b32 res args
emitPrimOp dflags res ReadByteArrayOp_Int              args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Word             args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Addr             args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp _      res ReadByteArrayOp_Float            args = doIndexByteArrayOp   Nothing f32 res args
emitPrimOp _      res ReadByteArrayOp_Double           args = doIndexByteArrayOp   Nothing f64 res args
emitPrimOp dflags res ReadByteArrayOp_StablePtr        args = doIndexByteArrayOp   Nothing (bWord dflags) res args
emitPrimOp dflags res ReadByteArrayOp_Int8             args = doIndexByteArrayOp   (Just (mo_s_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadByteArrayOp_Int16            args = doIndexByteArrayOp   (Just (mo_s_16ToWord dflags)) b16  res args
emitPrimOp dflags res ReadByteArrayOp_Int32            args = doIndexByteArrayOp   (Just (mo_s_32ToWord dflags)) b32  res args
emitPrimOp _      res ReadByteArrayOp_Int64            args = doIndexByteArrayOp   Nothing b64  res args
emitPrimOp dflags res ReadByteArrayOp_Word8            args = doIndexByteArrayOp   (Just (mo_u_8ToWord dflags)) b8  res args
emitPrimOp dflags res ReadByteArrayOp_Word16           args = doIndexByteArrayOp   (Just (mo_u_16ToWord dflags)) b16  res args
emitPrimOp dflags res ReadByteArrayOp_Word32           args = doIndexByteArrayOp   (Just (mo_u_32ToWord dflags)) b32  res args
emitPrimOp _      res ReadByteArrayOp_Word64           args = doIndexByteArrayOp   Nothing b64  res args
emitPrimOp _      res ReadByteArrayOp_FloatX4          args = doIndexByteArrayOp   Nothing vec4f32 res args
emitPrimOp _      res ReadByteArrayOp_FloatAsFloatX4   args = doIndexByteArrayOpAs Nothing vec4f32 f32 res args
emitPrimOp _      res ReadByteArrayOp_DoubleX2         args = doIndexByteArrayOp   Nothing vec2f64 res args
emitPrimOp _      res ReadByteArrayOp_DoubleAsDoubleX2 args = doIndexByteArrayOpAs Nothing vec2f64 f64 res args
emitPrimOp _      res ReadByteArrayOp_Int32X4          args = doIndexByteArrayOp   Nothing vec4b32 res args
emitPrimOp _      res ReadByteArrayOp_Int32AsInt32X4   args = doIndexByteArrayOpAs Nothing vec4b32 b32 res args
emitPrimOp _      res ReadByteArrayOp_Int64X2          args = doIndexByteArrayOp   Nothing vec2b64 res args
emitPrimOp _      res ReadByteArrayOp_Int64AsInt64X2   args = doIndexByteArrayOpAs Nothing vec2b64 b64 res args

-- WriteXXXoffAddr

emitPrimOp dflags res WriteOffAddrOp_Char             args = doWriteOffAddrOp (Just (mo_WordTo8 dflags))  b8 res args
emitPrimOp dflags res WriteOffAddrOp_WideChar         args = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp dflags res WriteOffAddrOp_Int              args = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Word             args = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Addr             args = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp _      res WriteOffAddrOp_Float            args = doWriteOffAddrOp Nothing f32 res args
emitPrimOp _      res WriteOffAddrOp_Double           args = doWriteOffAddrOp Nothing f64 res args
emitPrimOp dflags res WriteOffAddrOp_StablePtr        args = doWriteOffAddrOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteOffAddrOp_Int8             args = doWriteOffAddrOp (Just (mo_WordTo8 dflags))  b8 res args
emitPrimOp dflags res WriteOffAddrOp_Int16            args = doWriteOffAddrOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteOffAddrOp_Int32            args = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteOffAddrOp_Int64            args = doWriteOffAddrOp Nothing b64 res args
emitPrimOp dflags res WriteOffAddrOp_Word8            args = doWriteOffAddrOp (Just (mo_WordTo8 dflags))  b8 res args
emitPrimOp dflags res WriteOffAddrOp_Word16           args = doWriteOffAddrOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteOffAddrOp_Word32           args = doWriteOffAddrOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteOffAddrOp_Word64           args = doWriteOffAddrOp Nothing b64 res args
emitPrimOp _      res WriteOffAddrOp_FloatX4          args = doWriteOffAddrOp Nothing vec4f32 res args
emitPrimOp _      res WriteOffAddrOp_FloatAsFloatX4   args = doWriteOffAddrOp Nothing f32 res args
emitPrimOp _      res WriteOffAddrOp_DoubleX2         args = doWriteOffAddrOp Nothing vec2f64 res args
emitPrimOp _      res WriteOffAddrOp_DoubleAsDoubleX2 args = doWriteOffAddrOp Nothing f64 res args
emitPrimOp _      res WriteOffAddrOp_Int32X4          args = doWriteOffAddrOp Nothing vec4b32 res args
emitPrimOp _      res WriteOffAddrOp_Int32AsInt32X4   args = doWriteOffAddrOp Nothing b32 res args
emitPrimOp _      res WriteOffAddrOp_Int64X2          args = doWriteOffAddrOp Nothing vec2b64 res args
emitPrimOp _      res WriteOffAddrOp_Int64AsInt64X2   args = doWriteOffAddrOp Nothing b64 res args

-- WriteXXXArray

emitPrimOp dflags res WriteByteArrayOp_Char             args = doWriteByteArrayOp (Just (mo_WordTo8 dflags))  b8 res args
emitPrimOp dflags res WriteByteArrayOp_WideChar         args = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp dflags res WriteByteArrayOp_Int              args = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Word             args = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Addr             args = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp _      res WriteByteArrayOp_Float            args = doWriteByteArrayOp Nothing f32 res args
emitPrimOp _      res WriteByteArrayOp_Double           args = doWriteByteArrayOp Nothing f64 res args
emitPrimOp dflags res WriteByteArrayOp_StablePtr        args = doWriteByteArrayOp Nothing (bWord dflags) res args
emitPrimOp dflags res WriteByteArrayOp_Int8             args = doWriteByteArrayOp (Just (mo_WordTo8 dflags))  b8 res args
emitPrimOp dflags res WriteByteArrayOp_Int16            args = doWriteByteArrayOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteByteArrayOp_Int32            args = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteByteArrayOp_Int64            args = doWriteByteArrayOp Nothing b64 res args
emitPrimOp dflags res WriteByteArrayOp_Word8            args = doWriteByteArrayOp (Just (mo_WordTo8 dflags))  b8  res args
emitPrimOp dflags res WriteByteArrayOp_Word16           args = doWriteByteArrayOp (Just (mo_WordTo16 dflags)) b16 res args
emitPrimOp dflags res WriteByteArrayOp_Word32           args = doWriteByteArrayOp (Just (mo_WordTo32 dflags)) b32 res args
emitPrimOp _      res WriteByteArrayOp_Word64           args = doWriteByteArrayOp Nothing b64 res args
emitPrimOp _      res WriteByteArrayOp_FloatX4          args = doWriteByteArrayOp Nothing vec4f32 res args
emitPrimOp _      res WriteByteArrayOp_FloatAsFloatX4   args = doWriteByteArrayOp Nothing f32 res args
emitPrimOp _      res WriteByteArrayOp_DoubleX2         args = doWriteByteArrayOp Nothing vec2f64 res args
emitPrimOp _      res WriteByteArrayOp_DoubleAsDoubleX2 args = doWriteByteArrayOp Nothing f64 res args
emitPrimOp _      res WriteByteArrayOp_Int32X4          args = doWriteByteArrayOp Nothing vec4b32 res args
emitPrimOp _      res WriteByteArrayOp_Int32AsInt32X4   args = doWriteByteArrayOp Nothing b32 res args
emitPrimOp _      res WriteByteArrayOp_Int64X2          args = doWriteByteArrayOp Nothing vec2b64 res args
emitPrimOp _      res WriteByteArrayOp_Int64AsInt64X2   args = doWriteByteArrayOp Nothing b64 res args

-- Copying and setting byte arrays
emitPrimOp _      [] CopyByteArrayOp [src,src_off,dst,dst_off,n] =
    doCopyByteArrayOp src src_off dst dst_off n
emitPrimOp _      [] CopyMutableByteArrayOp [src,src_off,dst,dst_off,n] =
    doCopyMutableByteArrayOp src src_off dst dst_off n
emitPrimOp _      [] SetByteArrayOp [ba,off,len,c] =
    doSetByteArrayOp ba off len c

-- Population count
emitPrimOp _      [res] PopCnt8Op  [w] = emitPopCntCall res w W8
emitPrimOp _      [res] PopCnt16Op [w] = emitPopCntCall res w W16
emitPrimOp _      [res] PopCnt32Op [w] = emitPopCntCall res w W32
emitPrimOp _      [res] PopCnt64Op [w] = emitPopCntCall res w W64
emitPrimOp dflags [res] PopCntOp   [w] = emitPopCntCall res w (wordWidth dflags)

-- Unsigned int to floating point conversions
emitPrimOp _      [res] Word2FloatOp  [w] = emitPrimCall [res]
                                            (MO_UF_Conv W32) [w]
emitPrimOp _      [res] Word2DoubleOp [w] = emitPrimCall [res]
                                            (MO_UF_Conv W64) [w]

-- SIMD vector packing and unpacking
emitPrimOp _ [res] FloatToFloatX4Op [e] =
    doVecPackOp Nothing vec4f32 zero [e,e,e,e] res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 4 (CmmFloat 0 W32))

emitPrimOp _ [res] FloatX4PackOp es@[_,_,_,_] =
    doVecPackOp Nothing vec4f32 zero es res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 4 (CmmFloat 0 W32))

emitPrimOp _ res@[_,_,_,_] FloatX4UnpackOp [arg] =
    doVecUnpackOp Nothing vec4f32 arg res

emitPrimOp _ [res] FloatX4InsertOp [v,e,i] =
    doVecInsertOp Nothing vec4f32 v e i res

emitPrimOp _ [res] DoubleToDoubleX2Op [e] =
    doVecPackOp Nothing vec2f64 zero [e,e] res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 2 (CmmFloat 0 W64))

emitPrimOp _ [res] DoubleX2PackOp es@[_,_] =
    doVecPackOp Nothing vec2f64 zero es res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 2 (CmmFloat 0 W64))

emitPrimOp _ res@[_,_] DoubleX2UnpackOp [arg] =
    doVecUnpackOp Nothing vec2f64 arg res

emitPrimOp _ [res] DoubleX2InsertOp [v,e,i] =
    doVecInsertOp Nothing vec2f64 v e i res

emitPrimOp dflags [res] Int32ToInt32X4Op [e] =
    doVecPackOp (Just (mo_WordTo32 dflags)) vec4b32 zero [e,e,e,e] res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 4 (CmmInt 0 W32))

emitPrimOp dflags [res] Int32X4PackOp es@[_,_,_,_] =
    doVecPackOp (Just (mo_WordTo32 dflags)) vec4b32 zero es res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 4 (CmmInt 0 W32))

emitPrimOp dflags res@[_,_,_,_] Int32X4UnpackOp [arg] =
    doVecUnpackOp (Just (mo_s_32ToWord dflags)) vec4b32 arg res

emitPrimOp dflags [res] Int32X4InsertOp [v,e,i] =
    doVecInsertOp (Just (mo_WordTo32 dflags)) vec4b32 v e i res

emitPrimOp _ [res] Int64ToInt64X2Op [e] =
    doVecPackOp Nothing vec2b64 zero [e,e] res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 2 (CmmInt 0 W64))

emitPrimOp _ [res] Int64X2PackOp es@[_,_] =
    doVecPackOp Nothing vec2b64 zero es res
  where
    zero :: CmmExpr
    zero = CmmLit $ CmmVec (replicate 2 (CmmInt 0 W64))

emitPrimOp _ res@[_,_] Int64X2UnpackOp [arg] =
    doVecUnpackOp Nothing vec2b64 arg res

emitPrimOp _ [res] Int64X2InsertOp [v,e,i] =
    doVecInsertOp Nothing vec2b64 v e i res

-- The rest just translate straightforwardly
emitPrimOp dflags [res] op [arg]
   | nopOp op
   = emitAssign (CmmLocal res) arg

   | Just (mop,rep) <- narrowOp op
   = emitAssign (CmmLocal res) $
           CmmMachOp (mop rep (wordWidth dflags)) [CmmMachOp (mop (wordWidth dflags) rep) [arg]]

emitPrimOp dflags r@[res] op args
   | Just prim <- callishOp op
   = do emitPrimCall r prim args

   | Just mop <- translateOp dflags op
   = let stmt = mkAssign (CmmLocal res) (CmmMachOp mop args) in
     emit stmt

emitPrimOp dflags results op args
   = case callishPrimOpSupported dflags op of
          Left op   -> emit $ mkUnsafeCall (PrimTarget op) results args
          Right gen -> gen results args

type GenericOp = [CmmFormal] -> [CmmActual] -> FCode ()

callishPrimOpSupported :: DynFlags -> PrimOp -> Either CallishMachOp GenericOp
callishPrimOpSupported dflags op
  = case op of
      IntQuotRemOp   | ncg && x86ish  -> Left (MO_S_QuotRem  (wordWidth dflags))
                     | otherwise      -> Right (genericIntQuotRemOp dflags)

      WordQuotRemOp  | ncg && x86ish  -> Left (MO_U_QuotRem  (wordWidth dflags))
                     | otherwise      -> Right (genericWordQuotRemOp dflags)

      WordQuotRem2Op | ncg && x86ish  -> Left (MO_U_QuotRem2 (wordWidth dflags))
                     | otherwise      -> Right (genericWordQuotRem2Op dflags)

      WordAdd2Op     | ncg && x86ish  -> Left (MO_Add2       (wordWidth dflags))
                     | otherwise      -> Right genericWordAdd2Op

      WordMul2Op     | ncg && x86ish  -> Left (MO_U_Mul2     (wordWidth dflags))
                     | otherwise      -> Right genericWordMul2Op

      _ -> panic "emitPrimOp: can't translate PrimOp" (ppr op)
 where
  ncg = case hscTarget dflags of
           HscAsm -> True
           _      -> False

  x86ish = case platformArch (targetPlatform dflags) of
             ArchX86    -> True
             ArchX86_64 -> True
             _          -> False

genericIntQuotRemOp :: DynFlags -> GenericOp
genericIntQuotRemOp dflags [res_q, res_r] [arg_x, arg_y]
   = emit $ mkAssign (CmmLocal res_q)
              (CmmMachOp (MO_S_Quot (wordWidth dflags)) [arg_x, arg_y]) <*>
            mkAssign (CmmLocal res_r)
              (CmmMachOp (MO_S_Rem  (wordWidth dflags)) [arg_x, arg_y])
genericIntQuotRemOp _ _ _ = panic "genericIntQuotRemOp"

genericWordQuotRemOp :: DynFlags -> GenericOp
genericWordQuotRemOp dflags [res_q, res_r] [arg_x, arg_y]
    = emit $ mkAssign (CmmLocal res_q)
               (CmmMachOp (MO_U_Quot (wordWidth dflags)) [arg_x, arg_y]) <*>
             mkAssign (CmmLocal res_r)
               (CmmMachOp (MO_U_Rem  (wordWidth dflags)) [arg_x, arg_y])
genericWordQuotRemOp _ _ _ = panic "genericWordQuotRemOp"

genericWordQuotRem2Op :: DynFlags -> GenericOp
genericWordQuotRem2Op dflags [res_q, res_r] [arg_x_high, arg_x_low, arg_y]
    = emit =<< f (widthInBits (wordWidth dflags)) zero arg_x_high arg_x_low
    where    ty = cmmExprType dflags arg_x_high
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

             f :: Int -> CmmExpr -> CmmExpr -> CmmExpr -> FCode CmmAGraph
             f 0 acc high _ = return (mkAssign (CmmLocal res_q) acc <*>
                                      mkAssign (CmmLocal res_r) high)
             f i acc high low =
                 do roverflowedBit <- newTemp ty
                    rhigh'         <- newTemp ty
                    rhigh''        <- newTemp ty
                    rlow'          <- newTemp ty
                    risge          <- newTemp ty
                    racc'          <- newTemp ty
                    let high'         = CmmReg (CmmLocal rhigh')
                        isge          = CmmReg (CmmLocal risge)
                        overflowedBit = CmmReg (CmmLocal roverflowedBit)
                    let this = catAGraphs
                               [mkAssign (CmmLocal roverflowedBit)
                                          (shr high negone),
                                mkAssign (CmmLocal rhigh')
                                          (or (shl high one) (shr low negone)),
                                mkAssign (CmmLocal rlow')
                                          (shl low one),
                                mkAssign (CmmLocal risge)
                                          (or (overflowedBit `ne` zero)
                                              (high' `ge` arg_y)),
                                mkAssign (CmmLocal rhigh'')
                                          (high' `minus` (arg_y `times` isge)),
                                mkAssign (CmmLocal racc')
                                          (or (shl acc one) isge)]
                    rest <- f (i - 1) (CmmReg (CmmLocal racc'))
                                      (CmmReg (CmmLocal rhigh''))
                                      (CmmReg (CmmLocal rlow'))
                    return (this <*> rest)
genericWordQuotRem2Op _ _ _ = panic "genericWordQuotRem2Op"

genericWordAdd2Op :: GenericOp
genericWordAdd2Op [res_h, res_l] [arg_x, arg_y]
  = do dflags <- getDynFlags
       r1 <- newTemp (cmmExprType dflags arg_x)
       r2 <- newTemp (cmmExprType dflags arg_x)
       let topHalf x = CmmMachOp (MO_U_Shr (wordWidth dflags)) [x, hww]
           toTopHalf x = CmmMachOp (MO_Shl (wordWidth dflags)) [x, hww]
           bottomHalf x = CmmMachOp (MO_And (wordWidth dflags)) [x, hwm]
           add x y = CmmMachOp (MO_Add (wordWidth dflags)) [x, y]
           or x y = CmmMachOp (MO_Or (wordWidth dflags)) [x, y]
           hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth dflags)))
                                (wordWidth dflags))
           hwm = CmmLit (CmmInt (halfWordMask dflags) (wordWidth dflags))
       emit $ catAGraphs
          [mkAssign (CmmLocal r1)
               (add (bottomHalf arg_x) (bottomHalf arg_y)),
           mkAssign (CmmLocal r2)
               (add (topHalf (CmmReg (CmmLocal r1)))
                    (add (topHalf arg_x) (topHalf arg_y))),
           mkAssign (CmmLocal res_h)
               (topHalf (CmmReg (CmmLocal r2))),
           mkAssign (CmmLocal res_l)
               (or (toTopHalf (CmmReg (CmmLocal r2)))
                   (bottomHalf (CmmReg (CmmLocal r1))))]
genericWordAdd2Op _ _ = panic "genericWordAdd2Op"

genericWordMul2Op :: GenericOp
genericWordMul2Op [res_h, res_l] [arg_x, arg_y]
 = do dflags <- getDynFlags
      let t = cmmExprType dflags arg_x
      xlyl <- liftM CmmLocal $ newTemp t
      xlyh <- liftM CmmLocal $ newTemp t
      xhyl <- liftM CmmLocal $ newTemp t
      r    <- liftM CmmLocal $ newTemp t
      -- This generic implementation is very simple and slow. We might
      -- well be able to do better, but for now this at least works.
      let topHalf x = CmmMachOp (MO_U_Shr (wordWidth dflags)) [x, hww]
          toTopHalf x = CmmMachOp (MO_Shl (wordWidth dflags)) [x, hww]
          bottomHalf x = CmmMachOp (MO_And (wordWidth dflags)) [x, hwm]
          add x y = CmmMachOp (MO_Add (wordWidth dflags)) [x, y]
          sum = foldl1 add
          mul x y = CmmMachOp (MO_Mul (wordWidth dflags)) [x, y]
          or x y = CmmMachOp (MO_Or (wordWidth dflags)) [x, y]
          hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth dflags)))
                               (wordWidth dflags))
          hwm = CmmLit (CmmInt (halfWordMask dflags) (wordWidth dflags))
      emit $ catAGraphs
             [mkAssign xlyl
                  (mul (bottomHalf arg_x) (bottomHalf arg_y)),
              mkAssign xlyh
                  (mul (bottomHalf arg_x) (topHalf arg_y)),
              mkAssign xhyl
                  (mul (topHalf arg_x) (bottomHalf arg_y)),
              mkAssign r
                  (sum [topHalf    (CmmReg xlyl),
                        bottomHalf (CmmReg xhyl),
                        bottomHalf (CmmReg xlyh)]),
              mkAssign (CmmLocal res_l)
                  (or (bottomHalf (CmmReg xlyl))
                      (toTopHalf (CmmReg r))),
              mkAssign (CmmLocal res_h)
                  (sum [mul (topHalf arg_x) (topHalf arg_y),
                        topHalf (CmmReg xhyl),
                        topHalf (CmmReg xlyh),
                        topHalf (CmmReg r)])]
genericWordMul2Op _ _ = panic "genericWordMul2Op"

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

-- Floating point vector ops

translateOp _ FloatX4AddOp  = Just (MO_VF_Add  4 W32)
translateOp _ FloatX4SubOp  = Just (MO_VF_Sub  4 W32)
translateOp _ FloatX4MulOp  = Just (MO_VF_Mul  4 W32)
translateOp _ FloatX4DivOp  = Just (MO_VF_Quot 4 W32)
translateOp _ FloatX4NegOp  = Just (MO_VF_Neg  4 W32)

translateOp _ DoubleX2AddOp  = Just (MO_VF_Add  2 W64)
translateOp _ DoubleX2SubOp  = Just (MO_VF_Sub  2 W64)
translateOp _ DoubleX2MulOp  = Just (MO_VF_Mul  2 W64)
translateOp _ DoubleX2DivOp  = Just (MO_VF_Quot 2 W64)
translateOp _ DoubleX2NegOp  = Just (MO_VF_Neg  2 W64)

translateOp _ Int32X4AddOp   = Just (MO_V_Add   4 W32)
translateOp _ Int32X4SubOp   = Just (MO_V_Sub   4 W32)
translateOp _ Int32X4MulOp   = Just (MO_V_Mul   4 W32)
translateOp _ Int32X4QuotOp  = Just (MO_VS_Quot 4 W32)
translateOp _ Int32X4RemOp   = Just (MO_VS_Rem  4 W32)
translateOp _ Int32X4NegOp   = Just (MO_VS_Neg  4 W32)

translateOp _ Int64X2AddOp   = Just (MO_V_Add   2 W64)
translateOp _ Int64X2SubOp   = Just (MO_V_Sub   2 W64)
translateOp _ Int64X2MulOp   = Just (MO_V_Mul   2 W64)
translateOp _ Int64X2QuotOp  = Just (MO_VS_Quot 2 W64)
translateOp _ Int64X2RemOp   = Just (MO_VS_Rem  2 W64)
translateOp _ Int64X2NegOp   = Just (MO_VS_Neg  2 W64)

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

doIndexOffAddrOp :: Maybe MachOp
                 -> CmmType
                 -> [LocalReg]
                 -> [CmmExpr]
                 -> FCode ()
doIndexOffAddrOp maybe_post_read_cast rep [res] [addr,idx]
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr rep idx
doIndexOffAddrOp _ _ _ _
   = panic "StgCmmPrim: doIndexOffAddrOp"

doIndexOffAddrOpAs :: Maybe MachOp
                   -> CmmType
                   -> CmmType 
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doIndexOffAddrOpAs maybe_post_read_cast rep idx_rep [res] [addr,idx]
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr idx_rep idx
doIndexOffAddrOpAs _ _ _ _ _
   = panic "StgCmmPrim: doIndexOffAddrOpAs"

doIndexByteArrayOp :: Maybe MachOp
                   -> CmmType
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doIndexByteArrayOp maybe_post_read_cast rep [res] [addr,idx]
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrWordsHdrSize dflags) maybe_post_read_cast rep res addr rep idx
doIndexByteArrayOp _ _ _ _ 
   = panic "StgCmmPrim: doIndexByteArrayOp"

doIndexByteArrayOpAs :: Maybe MachOp
                    -> CmmType
                    -> CmmType 
                    -> [LocalReg]
                    -> [CmmExpr]
                    -> FCode ()
doIndexByteArrayOpAs maybe_post_read_cast rep idx_rep [res] [addr,idx]
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrWordsHdrSize dflags) maybe_post_read_cast rep res addr idx_rep idx
doIndexByteArrayOpAs _ _ _ _ _ 
   = panic "StgCmmPrim: doIndexByteArrayOpAs"

doReadPtrArrayOp :: LocalReg
                 -> CmmExpr
                 -> CmmExpr
                 -> FCode ()
doReadPtrArrayOp res addr idx
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrPtrsHdrSize dflags) Nothing (gcWord dflags) res addr (gcWord dflags) idx

doWriteOffAddrOp :: Maybe MachOp
                 -> CmmType
                 -> [LocalReg]
                 -> [CmmExpr]
                 -> FCode ()
doWriteOffAddrOp maybe_pre_write_cast idx_ty [] [addr,idx,val]
   = mkBasicIndexedWrite 0 maybe_pre_write_cast addr idx_ty idx val
doWriteOffAddrOp _ _ _ _
   = panic "StgCmmPrim: doWriteOffAddrOp"

doWriteByteArrayOp :: Maybe MachOp
                   -> CmmType
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doWriteByteArrayOp maybe_pre_write_cast idx_ty [] [addr,idx,val]
   = do dflags <- getDynFlags
        mkBasicIndexedWrite (arrWordsHdrSize dflags) maybe_pre_write_cast addr idx_ty idx val
doWriteByteArrayOp _ _ _ _
   = panic "StgCmmPrim: doWriteByteArrayOp"

doWritePtrArrayOp :: CmmExpr
                  -> CmmExpr
                  -> CmmExpr
                  -> FCode ()
doWritePtrArrayOp addr idx val
  = do dflags <- getDynFlags
       let ty = cmmExprType dflags val
       mkBasicIndexedWrite (arrPtrsHdrSize dflags) Nothing addr ty idx val
       emit (setInfo addr (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))
  -- the write barrier.  We must write a byte into the mark table:
  -- bits8[a + header_size + StgMutArrPtrs_size(a) + x >> N]
       emit $ mkStore (
         cmmOffsetExpr dflags
          (cmmOffsetExprW dflags (cmmOffsetB dflags addr (arrPtrsHdrSize dflags))
                         (loadArrPtrsSize dflags addr))
          (CmmMachOp (mo_wordUShr dflags) [idx,
                                           mkIntExpr dflags (mUT_ARR_PTRS_CARD_BITS dflags)])
         ) (CmmLit (CmmInt 1 W8))

loadArrPtrsSize :: DynFlags -> CmmExpr -> CmmExpr
loadArrPtrsSize dflags addr = CmmLoad (cmmOffsetB dflags addr off) (bWord dflags)
 where off = fixedHdrSize dflags * wORD_SIZE dflags + oFFSET_StgMutArrPtrs_ptrs dflags

mkBasicIndexedRead :: ByteOff      -- Initial offset in bytes
                   -> Maybe MachOp -- Optional result cast
                   -> CmmType      -- Type of element we are accessing
                   -> LocalReg     -- Destination
                   -> CmmExpr      -- Base address
                   -> CmmType      -- Type of element by which we are indexing
                   -> CmmExpr      -- Index
                   -> FCode ()
mkBasicIndexedRead off Nothing ty res base idx_ty idx
   = do dflags <- getDynFlags
        emitAssign (CmmLocal res) (cmmLoadIndexOffExpr dflags off ty base idx_ty idx)
mkBasicIndexedRead off (Just cast) ty res base idx_ty idx
   = do dflags <- getDynFlags
        emitAssign (CmmLocal res) (CmmMachOp cast [
                                   cmmLoadIndexOffExpr dflags off ty base idx_ty idx])

mkBasicIndexedWrite :: ByteOff      -- Initial offset in bytes
                    -> Maybe MachOp -- Optional value cast
                    -> CmmExpr      -- Base address
                    -> CmmType      -- Type of element by which we are indexing
                    -> CmmExpr      -- Index
                    -> CmmExpr      -- Value to write
                    -> FCode ()
mkBasicIndexedWrite off Nothing base idx_ty idx val
   = do dflags <- getDynFlags
        emitStore (cmmIndexOffExpr dflags off (typeWidth idx_ty) base idx) val
mkBasicIndexedWrite off (Just cast) base idx_ty idx val
   = mkBasicIndexedWrite off Nothing base idx_ty idx (CmmMachOp cast [val])

-- ----------------------------------------------------------------------------
-- Misc utils

cmmIndexOffExpr :: DynFlags
                -> ByteOff  -- Initial offset in bytes
                -> Width    -- Width of element by which we are indexing
                -> CmmExpr  -- Base address
                -> CmmExpr  -- Index
                -> CmmExpr
cmmIndexOffExpr dflags off width base idx
   = cmmIndexExpr dflags width (cmmOffsetB dflags base off) idx

cmmLoadIndexOffExpr :: DynFlags
                    -> ByteOff  -- Initial offset in bytes
                    -> CmmType  -- Type of element we are accessing
                    -> CmmExpr  -- Base address
                    -> CmmType  -- Type of element by which we are indexing
                    -> CmmExpr  -- Index
                    -> CmmExpr
cmmLoadIndexOffExpr dflags off ty base idx_ty idx
   = CmmLoad (cmmIndexOffExpr dflags off (typeWidth idx_ty) base idx) ty

setInfo :: CmmExpr -> CmmExpr -> CmmAGraph
setInfo closure_ptr info_ptr = mkStore closure_ptr info_ptr

------------------------------------------------------------------------------
-- Helpers for translating vector packing and unpacking.

doVecPackOp :: Maybe MachOp  -- Cast from element to vector component
            -> CmmType       -- Type of vector
            -> CmmExpr       -- Initial vector
            -> [CmmExpr]     -- Elements
            -> CmmFormal     -- Destination for result
            -> FCode ()
doVecPackOp maybe_pre_write_cast ty z es res = do
    dst <- newTemp ty
    emitAssign (CmmLocal dst) z
    vecPack dst es 0
  where
    vecPack :: CmmFormal -> [CmmExpr] -> Int -> FCode ()
    vecPack src [] _ =
        emitAssign (CmmLocal res) (CmmReg (CmmLocal src))

    vecPack src (e : es) i = do
        dst <- newTemp ty
        emitAssign (CmmLocal dst) (CmmMachOp (MO_V_Insert len wid)
                                             [CmmReg (CmmLocal src), cast e, iLit])
        vecPack dst es (i + 1)
      where
        -- vector indices are always 32-bits
        iLit = CmmLit (CmmInt (toInteger i) W32)

    cast :: CmmExpr -> CmmExpr
    cast val = case maybe_pre_write_cast of
                 Nothing   -> val
                 Just cast -> CmmMachOp cast [val]

    len :: Length
    len = vecLength ty 

    wid :: Width
    wid = typeWidth (vecElemType ty)

doVecUnpackOp :: Maybe MachOp  -- Cast from vector component to element result
              -> CmmType       -- Type of vector
              -> CmmExpr       -- Vector
              -> [CmmFormal]   -- Element results
              -> FCode ()
doVecUnpackOp maybe_post_read_cast ty e res =
    vecUnpack res 0
  where
    vecUnpack :: [CmmFormal] -> Int -> FCode ()
    vecUnpack [] _ =
        return ()

    vecUnpack (r : rs) i = do
        emitAssign (CmmLocal r) (cast (CmmMachOp (MO_V_Extract len wid)
                                      [e, iLit]))
        vecUnpack rs (i + 1)
      where
        -- vector indices are always 32-bits
        iLit = CmmLit (CmmInt (toInteger i) W32)

    cast :: CmmExpr -> CmmExpr
    cast val = case maybe_post_read_cast of
                 Nothing   -> val
                 Just cast -> CmmMachOp cast [val]

    len :: Length
    len = vecLength ty 

    wid :: Width
    wid = typeWidth (vecElemType ty)

doVecInsertOp :: Maybe MachOp  -- Cast from element to vector component
              -> CmmType       -- Vector type
              -> CmmExpr       -- Source vector
              -> CmmExpr       -- Element
              -> CmmExpr       -- Index at which to insert element
              -> CmmFormal     -- Destination for result
              -> FCode ()
doVecInsertOp maybe_pre_write_cast ty src e idx res = do
    dflags <- getDynFlags
    -- vector indices are always 32-bits
    let idx' :: CmmExpr
        idx' = CmmMachOp (MO_SS_Conv (wordWidth dflags) W32) [idx]
    emitAssign (CmmLocal res) (CmmMachOp (MO_V_Insert len wid) [src, cast e, idx'])
  where
    cast :: CmmExpr -> CmmExpr
    cast val = case maybe_pre_write_cast of
                 Nothing   -> val
                 Just cast -> CmmMachOp cast [val]

    len :: Length
    len = vecLength ty 

    wid :: Width
    wid = typeWidth (vecElemType ty)

-- ----------------------------------------------------------------------------
-- Copying byte arrays

-- | Takes a source 'ByteArray#', an offset in the source array, a
-- destination 'MutableByteArray#', an offset into the destination
-- array, and the number of bytes to copy.  Copies the given number of
-- bytes from the source array to the destination array.
doCopyByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> FCode ()
doCopyByteArrayOp = emitCopyByteArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p bytes (mkIntExpr dflags 1)

-- | Takes a source 'MutableByteArray#', an offset in the source
-- array, a destination 'MutableByteArray#', an offset into the
-- destination array, and the number of bytes to copy.  Copies the
-- given number of bytes from the source array to the destination
-- array.
doCopyMutableByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                         -> FCode ()
doCopyMutableByteArrayOp = emitCopyByteArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes = do
        dflags <- getDynFlags
        [moveCall, cpyCall] <- forkAlts [
            getCode $ emitMemmoveCall dst_p src_p bytes (mkIntExpr dflags 1),
            getCode $ emitMemcpyCall  dst_p src_p bytes (mkIntExpr dflags 1)
            ]
        emit =<< mkCmmIfThenElse (cmmEqWord dflags src dst) moveCall cpyCall

emitCopyByteArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                      -> FCode ())
                  -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> FCode ()
emitCopyByteArray copy src src_off dst dst_off n = do
    dflags <- getDynFlags
    dst_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags dst (arrWordsHdrSize dflags)) dst_off
    src_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags src (arrWordsHdrSize dflags)) src_off
    copy src dst dst_p src_p n

-- ----------------------------------------------------------------------------
-- Setting byte arrays

-- | Takes a 'MutableByteArray#', an offset into the array, a length,
-- and a byte, and sets each of the selected bytes in the array to the
-- character.
doSetByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                 -> FCode ()
doSetByteArrayOp ba off len c
    = do dflags <- getDynFlags
         p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags ba (arrWordsHdrSize dflags)) off
         emitMemsetCall p c len (mkIntExpr dflags 1)

-- ----------------------------------------------------------------------------
-- Copying pointer arrays

-- EZY: This code has an unusually high amount of assignTemp calls, seen
-- nowhere else in the code generator.  This is mostly because these
-- "primitive" ops result in a surprisingly large amount of code.  It
-- will likely be worthwhile to optimize what is emitted here, so that
-- our optimization passes don't waste time repeatedly optimizing the
-- same bits of code.

-- More closely imitates 'assignTemp' from the old code generator, which
-- returns a CmmExpr rather than a LocalReg.
assignTempE :: CmmExpr -> FCode CmmExpr
assignTempE e = do
    t <- assignTemp e
    return (CmmReg (CmmLocal t))

-- | Takes a source 'Array#', an offset in the source array, a
-- destination 'MutableArray#', an offset into the destination array,
-- and the number of elements to copy.  Copies the given number of
-- elements from the source array to the destination array.
doCopyArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
              -> FCode ()
doCopyArrayOp = emitCopyArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p bytes (mkIntExpr dflags (wORD_SIZE dflags))


-- | Takes a source 'MutableArray#', an offset in the source array, a
-- destination 'MutableArray#', an offset into the destination array,
-- and the number of elements to copy.  Copies the given number of
-- elements from the source array to the destination array.
doCopyMutableArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                     -> FCode ()
doCopyMutableArrayOp = emitCopyArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes = do
        dflags <- getDynFlags
        [moveCall, cpyCall] <- forkAlts [
            getCode $ emitMemmoveCall dst_p src_p bytes (mkIntExpr dflags (wORD_SIZE dflags)),
            getCode $ emitMemcpyCall  dst_p src_p bytes (mkIntExpr dflags (wORD_SIZE dflags))
            ]
        emit =<< mkCmmIfThenElse (cmmEqWord dflags src dst) moveCall cpyCall

emitCopyArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> FCode ())
              -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
              -> FCode ()
emitCopyArray copy src0 src_off0 dst0 dst_off0 n0 = do
    dflags <- getDynFlags
    n       <- assignTempE n0
    nonzero <- getCode $ do
        -- Passed as arguments (be careful)
        src     <- assignTempE src0
        src_off <- assignTempE src_off0
        dst     <- assignTempE dst0
        dst_off <- assignTempE dst_off0

        -- Set the dirty bit in the header.
        emit (setInfo dst (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))

        dst_elems_p <- assignTempE $ cmmOffsetB dflags dst (arrPtrsHdrSize dflags)
        dst_p <- assignTempE $ cmmOffsetExprW dflags dst_elems_p dst_off
        src_p <- assignTempE $ cmmOffsetExprW dflags (cmmOffsetB dflags src (arrPtrsHdrSize dflags)) src_off
        bytes <- assignTempE $ cmmMulWord dflags n (mkIntExpr dflags (wORD_SIZE dflags))

        copy src dst dst_p src_p bytes

        -- The base address of the destination card table
        dst_cards_p <- assignTempE $ cmmOffsetExprW dflags dst_elems_p (loadArrPtrsSize dflags dst)

        emitSetCards dst_off dst_cards_p n

    emit =<< mkCmmIfThen (cmmNeWord dflags n (mkIntExpr dflags 0)) nonzero

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy.  Allocates a new array and
-- initializes it form the source array.
emitCloneArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> CmmExpr
               -> FCode ()
emitCloneArray info_p res_r src0 src_off0 n0 = do
    dflags <- getDynFlags
    let arrPtrsHdrSizeW dflags = mkIntExpr dflags (fixedHdrSize dflags +
                                     (sIZEOF_StgMutArrPtrs_NoHdr dflags `div` wORD_SIZE dflags))
        myCapability = cmmSubWord dflags (CmmReg baseReg) (mkIntExpr dflags (oFFSET_Capability_r dflags))
    -- Passed as arguments (be careful)
    src     <- assignTempE src0
    src_off <- assignTempE src_off0
    n       <- assignTempE n0

    card_bytes <- assignTempE $ cardRoundUp dflags n
    size <- assignTempE $ cmmAddWord dflags n (bytesToWordsRoundUp dflags card_bytes)
    words <- assignTempE $ cmmAddWord dflags (arrPtrsHdrSizeW dflags) size

    arr_r <- newTemp (bWord dflags)
    emitAllocateCall arr_r myCapability words
    tickyAllocPrim (mkIntExpr dflags (arrPtrsHdrSize dflags)) (cmmMulWord dflags n (wordSize dflags))
                   (zeroExpr dflags)

    let arr = CmmReg (CmmLocal arr_r)
    emitSetDynHdr arr (CmmLit (CmmLabel info_p)) curCCS
    emit $ mkStore (cmmOffsetB dflags arr (fixedHdrSize dflags * wORD_SIZE dflags +
                                           oFFSET_StgMutArrPtrs_ptrs dflags)) n
    emit $ mkStore (cmmOffsetB dflags arr (fixedHdrSize dflags * wORD_SIZE dflags +
                                           oFFSET_StgMutArrPtrs_size dflags)) size

    dst_p <- assignTempE $ cmmOffsetB dflags arr (arrPtrsHdrSize dflags)
    src_p <- assignTempE $ cmmOffsetExprW dflags (cmmOffsetB dflags src (arrPtrsHdrSize dflags))
             src_off

    emitMemcpyCall dst_p src_p (cmmMulWord dflags n (wordSize dflags)) (mkIntExpr dflags (wORD_SIZE dflags))

    emitMemsetCall (cmmOffsetExprW dflags dst_p n)
        (mkIntExpr dflags 1)
        card_bytes
        (mkIntExpr dflags (wORD_SIZE dflags))
    emit $ mkAssign (CmmLocal res_r) arr

-- | Takes and offset in the destination array, the base address of
-- the card table, and the number of elements affected (*not* the
-- number of cards). The number of elements may not be zero.
-- Marks the relevant cards as dirty.
emitSetCards :: CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitSetCards dst_start dst_cards_start n = do
    dflags <- getDynFlags
    start_card <- assignTempE $ card dflags dst_start
    let end_card = card dflags (cmmSubWord dflags (cmmAddWord dflags dst_start n) (mkIntExpr dflags 1))
    emitMemsetCall (cmmAddWord dflags dst_cards_start start_card)
        (mkIntExpr dflags 1)
        (cmmAddWord dflags (cmmSubWord dflags end_card start_card) (mkIntExpr dflags 1))
        (mkIntExpr dflags 1) -- no alignment (1 byte)

-- Convert an element index to a card index
card :: DynFlags -> CmmExpr -> CmmExpr
card dflags i = cmmUShrWord dflags i (mkIntExpr dflags (mUT_ARR_PTRS_CARD_BITS dflags))

-- Convert a number of elements to a number of cards, rounding up
cardRoundUp :: DynFlags -> CmmExpr -> CmmExpr
cardRoundUp dflags i = card dflags (cmmAddWord dflags i (mkIntExpr dflags ((1 `shiftL` mUT_ARR_PTRS_CARD_BITS dflags) - 1)))

bytesToWordsRoundUp :: DynFlags -> CmmExpr -> CmmExpr
bytesToWordsRoundUp dflags e = cmmQuotWord dflags (cmmAddWord dflags e (mkIntExpr dflags (wORD_SIZE dflags - 1)))
                                                  (wordSize dflags)

wordSize :: DynFlags -> CmmExpr
wordSize dflags = mkIntExpr dflags (wORD_SIZE dflags)

-- | Emit a call to @memcpy@.
emitMemcpyCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitMemcpyCall dst src n align = do
    emitPrimCall
        [ {-no results-} ]
        MO_Memcpy
        [ dst, src, n, align ]

-- | Emit a call to @memmove@.
emitMemmoveCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitMemmoveCall dst src n align = do
    emitPrimCall
        [ {- no results -} ]
        MO_Memmove
        [ dst, src, n, align ]

-- | Emit a call to @memset@.  The second argument must fit inside an
-- unsigned char.
emitMemsetCall :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitMemsetCall dst c n align = do
    emitPrimCall
        [ {- no results -} ]
        MO_Memset
        [ dst, c, n, align ]

-- | Emit a call to @allocate@.
emitAllocateCall :: LocalReg -> CmmExpr -> CmmExpr -> FCode ()
emitAllocateCall res cap n = do
    emitCCall
        [ (res, AddrHint) ]
        allocate
        [ (cap, AddrHint)
        , (n, NoHint)
        ]
  where
    allocate = CmmLit (CmmLabel (mkForeignLabel (fsLit "allocate") Nothing
                                 ForeignLabelInExternalPackage IsFunction))

emitPopCntCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitPopCntCall res x width = do
    emitPrimCall
        [ res ]
        (MO_PopCnt width)
        [ x ]
