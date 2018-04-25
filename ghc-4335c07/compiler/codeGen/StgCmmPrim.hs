{-# LANGUAGE CPP #-}

----------------------------------------------------------------------------
--
-- Stg to C--: primitive operations
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmPrim (
   cgOpApp,
   cgPrimOp, -- internal(ish), used by cgCase to get code for a
             -- comparison without also turning it into a Bool.
   shouldInlinePrimOp
 ) where

#include "HsVersions.h"

import GhcPrelude hiding ((<*>))

import StgCmmLayout
import StgCmmForeign
import StgCmmEnv
import StgCmmMonad
import StgCmmUtils
import StgCmmTicky
import StgCmmHeap
import StgCmmProf ( costCentreFrom, curCCS )

import DynFlags
import Platform
import BasicTypes
import BlockId
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
import FastString
import Outputable
import Util

import Data.Bits ((.&.), bit)
import Control.Monad (liftM, when, unless)

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

cgOpApp (StgPrimOp primop) args res_ty = do
    dflags <- getDynFlags
    cmm_args <- getNonVoidArgAmodes args
    case shouldInlinePrimOp dflags primop cmm_args of
        Nothing -> do  -- out-of-line
          let fun = CmmLit (CmmLabel (mkRtsPrimOpLabel primop))
          emitCall (NativeNodeCall, NativeReturn) fun cmm_args

        Just f  -- inline
          | ReturnsPrim VoidRep <- result_info
          -> do f []
                emitReturn []

          | ReturnsPrim rep <- result_info
          -> do dflags <- getDynFlags
                res <- newTemp (primRepCmmType dflags rep)
                f [res]
                emitReturn [CmmReg (CmmLocal res)]

          | ReturnsAlg tycon <- result_info, isUnboxedTupleTyCon tycon
          -> do (regs, _hints) <- newUnboxedTupleRegs res_ty
                f regs
                emitReturn (map (CmmReg . CmmLocal) regs)

          | otherwise -> panic "cgPrimop"
          where
             result_info = getPrimOpResultInfo primop

cgOpApp (StgPrimCallOp primcall) args _res_ty
  = do  { cmm_args <- getNonVoidArgAmodes args
        ; let fun = CmmLit (CmmLabel (mkPrimCallLabel primcall))
        ; emitCall (NativeNodeCall, NativeReturn) fun cmm_args }

-- | Interpret the argument as an unsigned value, assuming the value
-- is given in two-complement form in the given width.
--
-- Example: @asUnsigned W64 (-1)@ is 18446744073709551615.
--
-- This function is used to work around the fact that many array
-- primops take Int# arguments, but we interpret them as unsigned
-- quantities in the code gen. This means that we have to be careful
-- every time we work on e.g. a CmmInt literal that corresponds to the
-- array size, as it might contain a negative Integer value if the
-- user passed a value larger than 2^(wORD_SIZE_IN_BITS-1) as the Int#
-- literal.
asUnsigned :: Width -> Integer -> Integer
asUnsigned w n = n .&. (bit (widthInBits w) - 1)

-- TODO: Several primop implementations (e.g. 'doNewByteArrayOp') use
--     ByteOff (or some other fixed width signed type) to represent
--     array sizes or indices. This means that these will overflow for
--     large enough sizes.

-- | Decide whether an out-of-line primop should be replaced by an
-- inline implementation. This might happen e.g. if there's enough
-- static information, such as statically know arguments, to emit a
-- more efficient implementation inline.
--
-- Returns 'Nothing' if this primop should use its out-of-line
-- implementation (defined elsewhere) and 'Just' together with a code
-- generating function that takes the output regs as arguments
-- otherwise.
shouldInlinePrimOp :: DynFlags
                   -> PrimOp     -- ^ The primop
                   -> [CmmExpr]  -- ^ The primop arguments
                   -> Maybe ([LocalReg] -> FCode ())

shouldInlinePrimOp dflags NewByteArrayOp_Char [(CmmLit (CmmInt n w))]
  | asUnsigned w n <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> doNewByteArrayOp res (fromInteger n)

shouldInlinePrimOp dflags NewArrayOp [(CmmLit (CmmInt n w)), init]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] ->
      doNewArrayOp res (arrPtrsRep dflags (fromInteger n)) mkMAP_DIRTY_infoLabel
      [ (mkIntExpr dflags (fromInteger n),
         fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags)
      , (mkIntExpr dflags (nonHdrSizeW (arrPtrsRep dflags (fromInteger n))),
         fixedHdrSize dflags + oFFSET_StgMutArrPtrs_size dflags)
      ]
      (fromInteger n) init

shouldInlinePrimOp _ CopyArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopyArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp _ CopyMutableArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopyMutableArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp _ CopyArrayArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopyArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp _ CopyMutableArrayArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopyMutableArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp dflags CloneArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneArray mkMAP_FROZEN_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags CloneMutableArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneArray mkMAP_DIRTY_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags FreezeArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneArray mkMAP_FROZEN_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags ThawArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneArray mkMAP_DIRTY_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags NewSmallArrayOp [(CmmLit (CmmInt n w)), init]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] ->
      doNewArrayOp res (smallArrPtrsRep (fromInteger n)) mkSMAP_DIRTY_infoLabel
      [ (mkIntExpr dflags (fromInteger n),
         fixedHdrSize dflags + oFFSET_StgSmallMutArrPtrs_ptrs dflags)
      ]
      (fromInteger n) init

shouldInlinePrimOp _ CopySmallArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopySmallArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp _ CopySmallMutableArrayOp
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] =
        Just $ \ [] -> doCopySmallMutableArrayOp src src_off dst dst_off (fromInteger n)

shouldInlinePrimOp dflags CloneSmallArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneSmallArray mkSMAP_FROZEN_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags CloneSmallMutableArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneSmallArray mkSMAP_DIRTY_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags FreezeSmallArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneSmallArray mkSMAP_FROZEN_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags ThawSmallArrayOp [src, src_off, (CmmLit (CmmInt n w))]
  | wordsToBytes dflags (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags) =
      Just $ \ [res] -> emitCloneSmallArray mkSMAP_DIRTY_infoLabel res src src_off (fromInteger n)

shouldInlinePrimOp dflags primop args
  | primOpOutOfLine primop = Nothing
  | otherwise = Just $ \ regs -> emitPrimOp dflags regs primop args

-- TODO: Several primops, such as 'copyArray#', only have an inline
-- implementation (below) but could possibly have both an inline
-- implementation and an out-of-line implementation, just like
-- 'newArray#'. This would lower the amount of code generated,
-- hopefully without a performance impact (needs to be measured).

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

emitPrimOp _ [res] ParOp [arg]
  =
        -- for now, just implement this in a C function
        -- later, we might want to inline it.
    emitCCall
        [(res,NoHint)]
        (CmmLit (CmmLabel (mkForeignLabel (fsLit "newSpark") Nothing ForeignLabelInExternalPackage IsFunction)))
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
            (CmmLit (CmmLabel (mkForeignLabel (fsLit "newSpark") Nothing ForeignLabelInExternalPackage IsFunction)))
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
   = emitAssign (CmmLocal res) (cmmLoadIndexW dflags mutv (fixedHdrSizeW dflags) (gcWord dflags))

emitPrimOp dflags res@[] WriteMutVarOp [mutv,var]
   = do -- Without this write barrier, other CPUs may see this pointer before
        -- the writes for the closure it points to have occurred.
        emitPrimCall res MO_WriteBarrier []
        emitStore (cmmOffsetW dflags mutv (fixedHdrSizeW dflags)) var
        emitCCall
                [{-no results-}]
                (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
                [(CmmReg (CmmGlobal BaseReg), AddrHint), (mutv,AddrHint)]

--  #define sizzeofByteArrayzh(r,a) \
--     r = ((StgArrBytes *)(a))->bytes
emitPrimOp dflags [res] SizeofByteArrayOp [arg]
   = emit $ mkAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSizeW dflags) (bWord dflags))

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrBytes *)(a))->bytes
emitPrimOp dflags [res] SizeofMutableByteArrayOp [arg]
   = emitPrimOp dflags [res] SizeofByteArrayOp [arg]

--  #define getSizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrBytes *)(a))->bytes
emitPrimOp dflags [res] GetSizeofMutableByteArrayOp [arg]
   = emitAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSizeW dflags) (bWord dflags))


--  #define touchzh(o)                  /* nothing */
emitPrimOp _ res@[] TouchOp args@[_arg]
   = do emitPrimCall res MO_Touch args

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
emitPrimOp dflags [res] ByteArrayContents_Char [arg]
   = emitAssign (CmmLocal res) (cmmOffsetB dflags arg (arrWordsHdrSize dflags))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
emitPrimOp dflags [res] StableNameToIntOp [arg]
   = emitAssign (CmmLocal res) (cmmLoadIndexW dflags arg (fixedHdrSizeW dflags) (bWord dflags))

--  #define eqStableNamezh(r,sn1,sn2)                                   \
--    (r = (((StgStableName *)sn1)->sn == ((StgStableName *)sn2)->sn))
emitPrimOp dflags [res] EqStableNameOp [arg1,arg2]
   = emitAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [
                                   cmmLoadIndexW dflags arg1 (fixedHdrSizeW dflags) (bWord dflags),
                                   cmmLoadIndexW dflags arg2 (fixedHdrSizeW dflags) (bWord dflags)
                         ])

emitPrimOp dflags [res] ReallyUnsafePtrEqualityOp [arg1,arg2]
   = emitAssign (CmmLocal res) (CmmMachOp (mo_wordEq dflags) [arg1,arg2])

--  #define addrToHValuezh(r,a) r=(P_)a
emitPrimOp _      [res] AddrToAnyOp [arg]
   = emitAssign (CmmLocal res) arg

--  #define hvalueToAddrzh(r, a) r=(W_)a
emitPrimOp _      [res] AnyToAddrOp [arg]
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
   [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN0_infoLabel)),
     mkAssign (CmmLocal res) arg ]
emitPrimOp _      [res] UnsafeFreezeArrayArrayOp [arg]
   = emit $ catAGraphs
   [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN0_infoLabel)),
     mkAssign (CmmLocal res) arg ]
emitPrimOp _      [res] UnsafeFreezeSmallArrayOp [arg]
   = emit $ catAGraphs
   [ setInfo arg (CmmLit (CmmLabel mkSMAP_FROZEN0_infoLabel)),
     mkAssign (CmmLocal res) arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)       r=(a)
emitPrimOp _      [res] UnsafeFreezeByteArrayOp [arg]
   = emitAssign (CmmLocal res) arg

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

emitPrimOp _      [res] ReadSmallArrayOp  [obj,ix] = doReadSmallPtrArrayOp res obj ix
emitPrimOp _      [res] IndexSmallArrayOp [obj,ix] = doReadSmallPtrArrayOp res obj ix
emitPrimOp _      []  WriteSmallArrayOp [obj,ix,v] = doWriteSmallPtrArrayOp obj ix v

-- Getting the size of pointer arrays

emitPrimOp dflags [res] SizeofArrayOp [arg]
   = emit $ mkAssign (CmmLocal res) (cmmLoadIndexW dflags arg
    (fixedHdrSizeW dflags + bytesToWordsRoundUp dflags (oFFSET_StgMutArrPtrs_ptrs dflags))
        (bWord dflags))
emitPrimOp dflags [res] SizeofMutableArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]
emitPrimOp dflags [res] SizeofArrayArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]
emitPrimOp dflags [res] SizeofMutableArrayArrayOp [arg]
   = emitPrimOp dflags [res] SizeofArrayOp [arg]

emitPrimOp dflags [res] SizeofSmallArrayOp [arg] =
    emit $ mkAssign (CmmLocal res)
    (cmmLoadIndexW dflags arg
     (fixedHdrSizeW dflags + bytesToWordsRoundUp dflags (oFFSET_StgSmallMutArrPtrs_ptrs dflags))
        (bWord dflags))
emitPrimOp dflags [res] SizeofSmallMutableArrayOp [arg] =
    emitPrimOp dflags [res] SizeofSmallArrayOp [arg]

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

-- Copying and setting byte arrays
emitPrimOp _      [] CopyByteArrayOp [src,src_off,dst,dst_off,n] =
    doCopyByteArrayOp src src_off dst dst_off n
emitPrimOp _      [] CopyMutableByteArrayOp [src,src_off,dst,dst_off,n] =
    doCopyMutableByteArrayOp src src_off dst dst_off n
emitPrimOp _      [] CopyByteArrayToAddrOp [src,src_off,dst,n] =
    doCopyByteArrayToAddrOp src src_off dst n
emitPrimOp _      [] CopyMutableByteArrayToAddrOp [src,src_off,dst,n] =
    doCopyMutableByteArrayToAddrOp src src_off dst n
emitPrimOp _      [] CopyAddrToByteArrayOp [src,dst,dst_off,n] =
    doCopyAddrToByteArrayOp src dst dst_off n
emitPrimOp _      [] SetByteArrayOp [ba,off,len,c] =
    doSetByteArrayOp ba off len c

-- Comparing byte arrays
emitPrimOp _      [res] CompareByteArraysOp [ba1,ba1_off,ba2,ba2_off,n] =
    doCompareByteArraysOp res ba1 ba1_off ba2 ba2_off n

emitPrimOp _      [res] BSwap16Op [w] = emitBSwapCall res w W16
emitPrimOp _      [res] BSwap32Op [w] = emitBSwapCall res w W32
emitPrimOp _      [res] BSwap64Op [w] = emitBSwapCall res w W64
emitPrimOp dflags [res] BSwapOp   [w] = emitBSwapCall res w (wordWidth dflags)

-- Population count
emitPrimOp _      [res] PopCnt8Op  [w] = emitPopCntCall res w W8
emitPrimOp _      [res] PopCnt16Op [w] = emitPopCntCall res w W16
emitPrimOp _      [res] PopCnt32Op [w] = emitPopCntCall res w W32
emitPrimOp _      [res] PopCnt64Op [w] = emitPopCntCall res w W64
emitPrimOp dflags [res] PopCntOp   [w] = emitPopCntCall res w (wordWidth dflags)

-- count leading zeros
emitPrimOp _      [res] Clz8Op  [w] = emitClzCall res w W8
emitPrimOp _      [res] Clz16Op [w] = emitClzCall res w W16
emitPrimOp _      [res] Clz32Op [w] = emitClzCall res w W32
emitPrimOp _      [res] Clz64Op [w] = emitClzCall res w W64
emitPrimOp dflags [res] ClzOp   [w] = emitClzCall res w (wordWidth dflags)

-- count trailing zeros
emitPrimOp _      [res] Ctz8Op [w]  = emitCtzCall res w W8
emitPrimOp _      [res] Ctz16Op [w] = emitCtzCall res w W16
emitPrimOp _      [res] Ctz32Op [w] = emitCtzCall res w W32
emitPrimOp _      [res] Ctz64Op [w] = emitCtzCall res w W64
emitPrimOp dflags [res] CtzOp   [w] = emitCtzCall res w (wordWidth dflags)

-- Unsigned int to floating point conversions
emitPrimOp _      [res] Word2FloatOp  [w] = emitPrimCall [res]
                                            (MO_UF_Conv W32) [w]
emitPrimOp _      [res] Word2DoubleOp [w] = emitPrimCall [res]
                                            (MO_UF_Conv W64) [w]

-- SIMD primops
emitPrimOp dflags [res] (VecBroadcastOp vcat n w) [e] = do
    checkVecCompatibility dflags vcat n w
    doVecPackOp (vecElemInjectCast dflags vcat w) ty zeros (replicate n e) res
  where
    zeros :: CmmExpr
    zeros = CmmLit $ CmmVec (replicate n zero)

    zero :: CmmLit
    zero = case vcat of
             IntVec   -> CmmInt 0 w
             WordVec  -> CmmInt 0 w
             FloatVec -> CmmFloat 0 w

    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags [res] (VecPackOp vcat n w) es = do
    checkVecCompatibility dflags vcat n w
    when (es `lengthIsNot` n) $
        panic "emitPrimOp: VecPackOp has wrong number of arguments"
    doVecPackOp (vecElemInjectCast dflags vcat w) ty zeros es res
  where
    zeros :: CmmExpr
    zeros = CmmLit $ CmmVec (replicate n zero)

    zero :: CmmLit
    zero = case vcat of
             IntVec   -> CmmInt 0 w
             WordVec  -> CmmInt 0 w
             FloatVec -> CmmFloat 0 w

    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecUnpackOp vcat n w) [arg] = do
    checkVecCompatibility dflags vcat n w
    when (res `lengthIsNot` n) $
        panic "emitPrimOp: VecUnpackOp has wrong number of results"
    doVecUnpackOp (vecElemProjectCast dflags vcat w) ty arg res
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags [res] (VecInsertOp vcat n w) [v,e,i] = do
    checkVecCompatibility dflags vcat n w
    doVecInsertOp (vecElemInjectCast dflags vcat w) ty v e i res
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecIndexByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecReadByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecWriteByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doWriteByteArrayOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecIndexOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecReadOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecWriteOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doWriteOffAddrOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecVmmType vcat n w

emitPrimOp dflags res (VecIndexScalarByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOpAs Nothing vecty ty res args
  where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

emitPrimOp dflags res (VecReadScalarByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOpAs Nothing vecty ty res args
  where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

emitPrimOp dflags res (VecWriteScalarByteArrayOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doWriteByteArrayOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecCmmCat vcat w

emitPrimOp dflags res (VecIndexScalarOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOpAs Nothing vecty ty res args
  where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

emitPrimOp dflags res (VecReadScalarOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOpAs Nothing vecty ty res args
  where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

emitPrimOp dflags res (VecWriteScalarOffAddrOp vcat n w) args = do
    checkVecCompatibility dflags vcat n w
    doWriteOffAddrOp Nothing ty res args
  where
    ty :: CmmType
    ty = vecCmmCat vcat w

-- Prefetch
emitPrimOp _ [] PrefetchByteArrayOp3        args = doPrefetchByteArrayOp 3  args
emitPrimOp _ [] PrefetchMutableByteArrayOp3 args = doPrefetchMutableByteArrayOp 3  args
emitPrimOp _ [] PrefetchAddrOp3             args = doPrefetchAddrOp  3  args
emitPrimOp _ [] PrefetchValueOp3            args = doPrefetchValueOp 3 args

emitPrimOp _ [] PrefetchByteArrayOp2        args = doPrefetchByteArrayOp 2  args
emitPrimOp _ [] PrefetchMutableByteArrayOp2 args = doPrefetchMutableByteArrayOp 2  args
emitPrimOp _ [] PrefetchAddrOp2             args = doPrefetchAddrOp 2  args
emitPrimOp _ [] PrefetchValueOp2           args = doPrefetchValueOp 2 args

emitPrimOp _ [] PrefetchByteArrayOp1        args = doPrefetchByteArrayOp 1  args
emitPrimOp _ [] PrefetchMutableByteArrayOp1 args = doPrefetchMutableByteArrayOp 1  args
emitPrimOp _ [] PrefetchAddrOp1             args = doPrefetchAddrOp 1  args
emitPrimOp _ [] PrefetchValueOp1            args = doPrefetchValueOp 1 args

emitPrimOp _ [] PrefetchByteArrayOp0        args = doPrefetchByteArrayOp 0  args
emitPrimOp _ [] PrefetchMutableByteArrayOp0 args = doPrefetchMutableByteArrayOp 0  args
emitPrimOp _ [] PrefetchAddrOp0             args = doPrefetchAddrOp 0  args
emitPrimOp _ [] PrefetchValueOp0            args = doPrefetchValueOp 0 args

-- Atomic read-modify-write
emitPrimOp dflags [res] FetchAddByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_Add mba ix (bWord dflags) n
emitPrimOp dflags [res] FetchSubByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_Sub mba ix (bWord dflags) n
emitPrimOp dflags [res] FetchAndByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_And mba ix (bWord dflags) n
emitPrimOp dflags [res] FetchNandByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_Nand mba ix (bWord dflags) n
emitPrimOp dflags [res] FetchOrByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_Or mba ix (bWord dflags) n
emitPrimOp dflags [res] FetchXorByteArrayOp_Int [mba, ix, n] =
    doAtomicRMW res AMO_Xor mba ix (bWord dflags) n
emitPrimOp dflags [res] AtomicReadByteArrayOp_Int [mba, ix] =
    doAtomicReadByteArray res mba ix (bWord dflags)
emitPrimOp dflags [] AtomicWriteByteArrayOp_Int [mba, ix, val] =
    doAtomicWriteByteArray mba ix (bWord dflags) val
emitPrimOp dflags [res] CasByteArrayOp_Int [mba, ix, old, new] =
    doCasByteArray res mba ix (bWord dflags) old new

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
      IntQuotRemOp   | ncg && (x86ish
                              || ppc) -> Left (MO_S_QuotRem  (wordWidth dflags))
                     | otherwise      -> Right (genericIntQuotRemOp dflags)

      WordQuotRemOp  | ncg && (x86ish
                              || ppc) -> Left (MO_U_QuotRem  (wordWidth dflags))
                     | otherwise      -> Right (genericWordQuotRemOp dflags)

      WordQuotRem2Op | (ncg && (x86ish
                                || ppc))
                          || llvm     -> Left (MO_U_QuotRem2 (wordWidth dflags))
                     | otherwise      -> Right (genericWordQuotRem2Op dflags)

      WordAdd2Op     | (ncg && (x86ish
                                || ppc))
                         || llvm      -> Left (MO_Add2       (wordWidth dflags))
                     | otherwise      -> Right genericWordAdd2Op

      WordSubCOp     | (ncg && (x86ish
                                || ppc))
                         || llvm      -> Left (MO_SubWordC   (wordWidth dflags))
                     | otherwise      -> Right genericWordSubCOp

      IntAddCOp      | (ncg && (x86ish
                                || ppc))
                         || llvm      -> Left (MO_AddIntC    (wordWidth dflags))
                     | otherwise      -> Right genericIntAddCOp

      IntSubCOp      | (ncg && (x86ish
                                || ppc))
                         || llvm      -> Left (MO_SubIntC    (wordWidth dflags))
                     | otherwise      -> Right genericIntSubCOp

      WordMul2Op     | ncg && (x86ish
                               || ppc)
                         || llvm      -> Left (MO_U_Mul2     (wordWidth dflags))
                     | otherwise      -> Right genericWordMul2Op
      FloatFabsOp    | (ncg && x86ish
                               || ppc)
                         || llvm      -> Left MO_F32_Fabs
                     | otherwise      -> Right $ genericFabsOp W32
      DoubleFabsOp   | (ncg && x86ish
                               || ppc)
                         || llvm      -> Left MO_F64_Fabs
                     | otherwise      -> Right $ genericFabsOp W64

      _ -> pprPanic "emitPrimOp: can't translate PrimOp " (ppr op)
 where
  ncg = case hscTarget dflags of
           HscAsm -> True
           _      -> False
  llvm = case hscTarget dflags of
           HscLlvm -> True
           _       -> False
  x86ish = case platformArch (targetPlatform dflags) of
             ArchX86    -> True
             ArchX86_64 -> True
             _          -> False
  ppc = case platformArch (targetPlatform dflags) of
          ArchPPC      -> True
          ArchPPC_64 _ -> True
          _            -> False

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

genericWordSubCOp :: GenericOp
genericWordSubCOp [res_r, res_c] [aa, bb] = do
  dflags <- getDynFlags
  emit $ catAGraphs
    [ -- Put the result into 'res_r'.
      mkAssign (CmmLocal res_r) $
        CmmMachOp (mo_wordSub dflags) [aa, bb]
      -- Set 'res_c' to 1 if 'bb > aa' and to 0 otherwise.
    , mkAssign (CmmLocal res_c) $
        CmmMachOp (mo_wordUGt dflags) [bb, aa]
    ]
genericWordSubCOp _ _ = panic "genericWordSubCOp"

genericIntAddCOp :: GenericOp
genericIntAddCOp [res_r, res_c] [aa, bb]
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
 = do dflags <- getDynFlags
      emit $ catAGraphs [
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
genericIntAddCOp _ _ = panic "genericIntAddCOp"

genericIntSubCOp :: GenericOp
genericIntSubCOp [res_r, res_c] [aa, bb]
{- Similarly:
   #define subIntCzh(r,c,a,b)                                   \
   { r = ((I_)(a)) - ((I_)(b));                                 \
     c = ((StgWord)((((I_)(a))^((I_)(b))) & (((I_)(a))^r)))     \
         >> (BITS_IN (I_) - 1);                                 \
   }

   c =  ((a^b) & (a^r)) >>unsigned (BITS_IN(I_)-1)
-}
 = do dflags <- getDynFlags
      emit $ catAGraphs [
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
genericIntSubCOp _ _ = panic "genericIntSubCOp"

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

-- This replicates what we had in libraries/base/GHC/Float.hs:
--
--    abs x    | x == 0    = 0 -- handles (-0.0)
--             | x >  0    = x
--             | otherwise = negateFloat x
genericFabsOp :: Width -> GenericOp
genericFabsOp w [res_r] [aa]
 = do dflags <- getDynFlags
      let zero   = CmmLit (CmmFloat 0 w)

          eq x y = CmmMachOp (MO_F_Eq w) [x, y]
          gt x y = CmmMachOp (MO_F_Gt w) [x, y]

          neg x  = CmmMachOp (MO_F_Neg w) [x]

          g1 = catAGraphs [mkAssign (CmmLocal res_r) zero]
          g2 = catAGraphs [mkAssign (CmmLocal res_r) aa]

      res_t <- CmmLocal <$> newTemp (cmmExprType dflags aa)
      let g3 = catAGraphs [mkAssign res_t aa,
                           mkAssign (CmmLocal res_r) (neg (CmmReg res_t))]

      g4 <- mkCmmIfThenElse (gt aa zero) g2 g3

      emit =<< mkCmmIfThenElse (eq aa zero) g1 g4

genericFabsOp _ _ _ = panic "genericFabsOp"

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

translateOp dflags AndIOp         = Just (mo_wordAnd dflags)
translateOp dflags OrIOp          = Just (mo_wordOr dflags)
translateOp dflags XorIOp         = Just (mo_wordXor dflags)
translateOp dflags NotIOp         = Just (mo_wordNot dflags)
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

-- Vector ops

translateOp _ (VecAddOp FloatVec n w) = Just (MO_VF_Add  n w)
translateOp _ (VecSubOp FloatVec n w) = Just (MO_VF_Sub  n w)
translateOp _ (VecMulOp FloatVec n w) = Just (MO_VF_Mul  n w)
translateOp _ (VecDivOp FloatVec n w) = Just (MO_VF_Quot n w)
translateOp _ (VecNegOp FloatVec n w) = Just (MO_VF_Neg  n w)

translateOp _ (VecAddOp  IntVec n w) = Just (MO_V_Add   n w)
translateOp _ (VecSubOp  IntVec n w) = Just (MO_V_Sub   n w)
translateOp _ (VecMulOp  IntVec n w) = Just (MO_V_Mul   n w)
translateOp _ (VecQuotOp IntVec n w) = Just (MO_VS_Quot n w)
translateOp _ (VecRemOp  IntVec n w) = Just (MO_VS_Rem  n w)
translateOp _ (VecNegOp  IntVec n w) = Just (MO_VS_Neg  n w)

translateOp _ (VecAddOp  WordVec n w) = Just (MO_V_Add   n w)
translateOp _ (VecSubOp  WordVec n w) = Just (MO_V_Sub   n w)
translateOp _ (VecMulOp  WordVec n w) = Just (MO_V_Mul   n w)
translateOp _ (VecQuotOp WordVec n w) = Just (MO_VU_Quot n w)
translateOp _ (VecRemOp  WordVec n w) = Just (MO_VU_Rem  n w)

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
translateOp dflags SameSmallMutableArrayOp= Just (mo_wordEq dflags)
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
       -- This write barrier is to ensure that the heap writes to the object
       -- referred to by val have happened before we write val into the array.
       -- See #12469 for details.
       emitPrimCall [] MO_WriteBarrier []
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
 where off = fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags

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
-- Helpers for translating vector primops.

vecVmmType :: PrimOpVecCat -> Length -> Width -> CmmType
vecVmmType pocat n w = vec n (vecCmmCat pocat w)

vecCmmCat :: PrimOpVecCat -> Width -> CmmType
vecCmmCat IntVec   = cmmBits
vecCmmCat WordVec  = cmmBits
vecCmmCat FloatVec = cmmFloat

vecElemInjectCast :: DynFlags -> PrimOpVecCat -> Width -> Maybe MachOp
vecElemInjectCast _      FloatVec _   =  Nothing
vecElemInjectCast dflags IntVec   W8  =  Just (mo_WordTo8  dflags)
vecElemInjectCast dflags IntVec   W16 =  Just (mo_WordTo16 dflags)
vecElemInjectCast dflags IntVec   W32 =  Just (mo_WordTo32 dflags)
vecElemInjectCast _      IntVec   W64 =  Nothing
vecElemInjectCast dflags WordVec  W8  =  Just (mo_WordTo8  dflags)
vecElemInjectCast dflags WordVec  W16 =  Just (mo_WordTo16 dflags)
vecElemInjectCast dflags WordVec  W32 =  Just (mo_WordTo32 dflags)
vecElemInjectCast _      WordVec  W64 =  Nothing
vecElemInjectCast _      _        _   =  Nothing

vecElemProjectCast :: DynFlags -> PrimOpVecCat -> Width -> Maybe MachOp
vecElemProjectCast _      FloatVec _   =  Nothing
vecElemProjectCast dflags IntVec   W8  =  Just (mo_s_8ToWord  dflags)
vecElemProjectCast dflags IntVec   W16 =  Just (mo_s_16ToWord dflags)
vecElemProjectCast dflags IntVec   W32 =  Just (mo_s_32ToWord dflags)
vecElemProjectCast _      IntVec   W64 =  Nothing
vecElemProjectCast dflags WordVec  W8  =  Just (mo_u_8ToWord  dflags)
vecElemProjectCast dflags WordVec  W16 =  Just (mo_u_16ToWord dflags)
vecElemProjectCast dflags WordVec  W32 =  Just (mo_u_32ToWord dflags)
vecElemProjectCast _      WordVec  W64 =  Nothing
vecElemProjectCast _      _        _   =  Nothing

-- Check to make sure that we can generate code for the specified vector type
-- given the current set of dynamic flags.
checkVecCompatibility :: DynFlags -> PrimOpVecCat -> Length -> Width -> FCode ()
checkVecCompatibility dflags vcat l w = do
    when (hscTarget dflags /= HscLlvm) $ do
        sorry $ unlines ["SIMD vector instructions require the LLVM back-end."
                         ,"Please use -fllvm."]
    check vecWidth vcat l w
  where
    check :: Width -> PrimOpVecCat -> Length -> Width -> FCode ()
    check W128 FloatVec 4 W32 | not (isSseEnabled dflags) =
        sorry $ "128-bit wide single-precision floating point " ++
                "SIMD vector instructions require at least -msse."
    check W128 _ _ _ | not (isSse2Enabled dflags) =
        sorry $ "128-bit wide integer and double precision " ++
                "SIMD vector instructions require at least -msse2."
    check W256 FloatVec _ _ | not (isAvxEnabled dflags) =
        sorry $ "256-bit wide floating point " ++
                "SIMD vector instructions require at least -mavx."
    check W256 _ _ _ | not (isAvx2Enabled dflags) =
        sorry $ "256-bit wide integer " ++
                "SIMD vector instructions require at least -mavx2."
    check W512 _ _ _ | not (isAvx512fEnabled dflags) =
        sorry $ "512-bit wide " ++
                "SIMD vector instructions require -mavx512f."
    check _ _ _ _ = return ()

    vecWidth = typeWidth (vecVmmType vcat l w)

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
        if isFloatType (vecElemType ty)
          then emitAssign (CmmLocal dst) (CmmMachOp (MO_VF_Insert len wid)
                                                    [CmmReg (CmmLocal src), cast e, iLit])
          else emitAssign (CmmLocal dst) (CmmMachOp (MO_V_Insert len wid)
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
        if isFloatType (vecElemType ty)
          then emitAssign (CmmLocal r) (cast (CmmMachOp (MO_VF_Extract len wid)
                                             [e, iLit]))
          else emitAssign (CmmLocal r) (cast (CmmMachOp (MO_V_Extract len wid)
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
    if isFloatType (vecElemType ty)
      then emitAssign (CmmLocal res) (CmmMachOp (MO_VF_Insert len wid) [src, cast e, idx'])
      else emitAssign (CmmLocal res) (CmmMachOp (MO_V_Insert len wid) [src, cast e, idx'])
  where
    cast :: CmmExpr -> CmmExpr
    cast val = case maybe_pre_write_cast of
                 Nothing   -> val
                 Just cast -> CmmMachOp cast [val]

    len :: Length
    len = vecLength ty

    wid :: Width
    wid = typeWidth (vecElemType ty)

------------------------------------------------------------------------------
-- Helpers for translating prefetching.


-- | Translate byte array prefetch operations into proper primcalls.
doPrefetchByteArrayOp :: Int
                      -> [CmmExpr]
                      -> FCode ()
doPrefetchByteArrayOp locality  [addr,idx]
   = do dflags <- getDynFlags
        mkBasicPrefetch locality (arrWordsHdrSize dflags)  addr idx
doPrefetchByteArrayOp _ _
   = panic "StgCmmPrim: doPrefetchByteArrayOp"

-- | Translate mutable byte array prefetch operations into proper primcalls.
doPrefetchMutableByteArrayOp :: Int
                      -> [CmmExpr]
                      -> FCode ()
doPrefetchMutableByteArrayOp locality  [addr,idx]
   = do dflags <- getDynFlags
        mkBasicPrefetch locality (arrWordsHdrSize dflags)  addr idx
doPrefetchMutableByteArrayOp _ _
   = panic "StgCmmPrim: doPrefetchByteArrayOp"

-- | Translate address prefetch operations into proper primcalls.
doPrefetchAddrOp ::Int
                 -> [CmmExpr]
                 -> FCode ()
doPrefetchAddrOp locality   [addr,idx]
   = mkBasicPrefetch locality 0  addr idx
doPrefetchAddrOp _ _
   = panic "StgCmmPrim: doPrefetchAddrOp"

-- | Translate value prefetch operations into proper primcalls.
doPrefetchValueOp :: Int
                 -> [CmmExpr]
                 -> FCode ()
doPrefetchValueOp  locality   [addr]
  =  do dflags <- getDynFlags
        mkBasicPrefetch locality 0 addr  (CmmLit (CmmInt 0 (wordWidth dflags)))
doPrefetchValueOp _ _
  = panic "StgCmmPrim: doPrefetchValueOp"

-- | helper to generate prefetch primcalls
mkBasicPrefetch :: Int          -- Locality level 0-3
                -> ByteOff      -- Initial offset in bytes
                -> CmmExpr      -- Base address
                -> CmmExpr      -- Index
                -> FCode ()
mkBasicPrefetch locality off base idx
   = do dflags <- getDynFlags
        emitPrimCall [] (MO_Prefetch_Data locality) [cmmIndexExpr dflags W8 (cmmOffsetB dflags base off) idx]
        return ()

-- ----------------------------------------------------------------------------
-- Allocating byte arrays

-- | Takes a register to return the newly allocated array in and the
-- size of the new array in bytes. Allocates a new
-- 'MutableByteArray#'.
doNewByteArrayOp :: CmmFormal -> ByteOff -> FCode ()
doNewByteArrayOp res_r n = do
    dflags <- getDynFlags

    let info_ptr = mkLblExpr mkArrWords_infoLabel
        rep = arrWordsRep dflags n

    tickyAllocPrim (mkIntExpr dflags (arrWordsHdrSize dflags))
        (mkIntExpr dflags (nonHdrSize dflags rep))
        (zeroExpr dflags)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr curCCS
                     [ (mkIntExpr dflags n,
                        hdr_size + oFFSET_StgArrBytes_bytes dflags)
                     ]

    emit $ mkAssign (CmmLocal res_r) base

-- ----------------------------------------------------------------------------
-- Comparing byte arrays

doCompareByteArraysOp :: LocalReg -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                     -> FCode ()
doCompareByteArraysOp res ba1 ba1_off ba2 ba2_off n = do
    dflags <- getDynFlags
    ba1_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags ba1 (arrWordsHdrSize dflags)) ba1_off
    ba2_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags ba2 (arrWordsHdrSize dflags)) ba2_off
    emitMemcmpCall res ba1_p ba2_p n 1

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
        emitMemcpyCall dst_p src_p bytes 1

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
            getCode $ emitMemmoveCall dst_p src_p bytes 1,
            getCode $ emitMemcpyCall  dst_p src_p bytes 1
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

-- | Takes a source 'ByteArray#', an offset in the source array, a
-- destination 'Addr#', and the number of bytes to copy.  Copies the given
-- number of bytes from the source array to the destination memory region.
doCopyByteArrayToAddrOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
doCopyByteArrayToAddrOp src src_off dst_p bytes = do
    -- Use memcpy (we are allowed to assume the arrays aren't overlapping)
    dflags <- getDynFlags
    src_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags src (arrWordsHdrSize dflags)) src_off
    emitMemcpyCall dst_p src_p bytes 1

-- | Takes a source 'MutableByteArray#', an offset in the source array, a
-- destination 'Addr#', and the number of bytes to copy.  Copies the given
-- number of bytes from the source array to the destination memory region.
doCopyMutableByteArrayToAddrOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                               -> FCode ()
doCopyMutableByteArrayToAddrOp = doCopyByteArrayToAddrOp

-- | Takes a source 'Addr#', a destination 'MutableByteArray#', an offset into
-- the destination array, and the number of bytes to copy.  Copies the given
-- number of bytes from the source memory region to the destination array.
doCopyAddrToByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
doCopyAddrToByteArrayOp src_p dst dst_off bytes = do
    -- Use memcpy (we are allowed to assume the arrays aren't overlapping)
    dflags <- getDynFlags
    dst_p <- assignTempE $ cmmOffsetExpr dflags (cmmOffsetB dflags dst (arrWordsHdrSize dflags)) dst_off
    emitMemcpyCall dst_p src_p bytes 1


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
         emitMemsetCall p c len 1

-- ----------------------------------------------------------------------------
-- Allocating arrays

-- | Allocate a new array.
doNewArrayOp :: CmmFormal             -- ^ return register
             -> SMRep                 -- ^ representation of the array
             -> CLabel                -- ^ info pointer
             -> [(CmmExpr, ByteOff)]  -- ^ header payload
             -> WordOff               -- ^ array size
             -> CmmExpr               -- ^ initial element
             -> FCode ()
doNewArrayOp res_r rep info payload n init = do
    dflags <- getDynFlags

    let info_ptr = mkLblExpr info

    tickyAllocPrim (mkIntExpr dflags (hdrSize dflags rep))
        (mkIntExpr dflags (nonHdrSize dflags rep))
        (zeroExpr dflags)

    base <- allocHeapClosure rep info_ptr curCCS payload

    arr <- CmmLocal `fmap` newTemp (bWord dflags)
    emit $ mkAssign arr base

    -- Initialise all elements of the the array
    p <- assignTemp $ cmmOffsetB dflags (CmmReg arr) (hdrSize dflags rep)
    for <- newBlockId
    emitLabel for
    let loopBody =
            [ mkStore (CmmReg (CmmLocal p)) init
            , mkAssign (CmmLocal p) (cmmOffsetW dflags (CmmReg (CmmLocal p)) 1)
            , mkBranch for ]
    emit =<< mkCmmIfThen
        (cmmULtWord dflags (CmmReg (CmmLocal p))
         (cmmOffsetW dflags (CmmReg arr)
          (hdrSizeW dflags rep + n)))
        (catAGraphs loopBody)

    emit $ mkAssign (CmmLocal res_r) (CmmReg arr)

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
doCopyArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
              -> FCode ()
doCopyArrayOp = emitCopyArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p (mkIntExpr dflags bytes)
               (wORD_SIZE dflags)


-- | Takes a source 'MutableArray#', an offset in the source array, a
-- destination 'MutableArray#', an offset into the destination array,
-- and the number of elements to copy.  Copies the given number of
-- elements from the source array to the destination array.
doCopyMutableArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
                     -> FCode ()
doCopyMutableArrayOp = emitCopyArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes = do
        dflags <- getDynFlags
        [moveCall, cpyCall] <- forkAlts [
            getCode $ emitMemmoveCall dst_p src_p (mkIntExpr dflags bytes)
            (wORD_SIZE dflags),
            getCode $ emitMemcpyCall  dst_p src_p (mkIntExpr dflags bytes)
            (wORD_SIZE dflags)
            ]
        emit =<< mkCmmIfThenElse (cmmEqWord dflags src dst) moveCall cpyCall

emitCopyArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> ByteOff
                  -> FCode ())  -- ^ copy function
              -> CmmExpr        -- ^ source array
              -> CmmExpr        -- ^ offset in source array
              -> CmmExpr        -- ^ destination array
              -> CmmExpr        -- ^ offset in destination array
              -> WordOff        -- ^ number of elements to copy
              -> FCode ()
emitCopyArray copy src0 src_off dst0 dst_off0 n = do
    dflags <- getDynFlags
    when (n /= 0) $ do
        -- Passed as arguments (be careful)
        src     <- assignTempE src0
        dst     <- assignTempE dst0
        dst_off <- assignTempE dst_off0

        -- Set the dirty bit in the header.
        emit (setInfo dst (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))

        dst_elems_p <- assignTempE $ cmmOffsetB dflags dst
                       (arrPtrsHdrSize dflags)
        dst_p <- assignTempE $ cmmOffsetExprW dflags dst_elems_p dst_off
        src_p <- assignTempE $ cmmOffsetExprW dflags
                 (cmmOffsetB dflags src (arrPtrsHdrSize dflags)) src_off
        let bytes = wordsToBytes dflags n

        copy src dst dst_p src_p bytes

        -- The base address of the destination card table
        dst_cards_p <- assignTempE $ cmmOffsetExprW dflags dst_elems_p
                       (loadArrPtrsSize dflags dst)

        emitSetCards dst_off dst_cards_p n

doCopySmallArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
                   -> FCode ()
doCopySmallArrayOp = emitCopySmallArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes =
        do dflags <- getDynFlags
           emitMemcpyCall dst_p src_p (mkIntExpr dflags bytes)
               (wORD_SIZE dflags)


doCopySmallMutableArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
                          -> FCode ()
doCopySmallMutableArrayOp = emitCopySmallArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes = do
        dflags <- getDynFlags
        [moveCall, cpyCall] <- forkAlts
            [ getCode $ emitMemmoveCall dst_p src_p (mkIntExpr dflags bytes)
              (wORD_SIZE dflags)
            , getCode $ emitMemcpyCall  dst_p src_p (mkIntExpr dflags bytes)
              (wORD_SIZE dflags)
            ]
        emit =<< mkCmmIfThenElse (cmmEqWord dflags src dst) moveCall cpyCall

emitCopySmallArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> ByteOff
                       -> FCode ())  -- ^ copy function
                   -> CmmExpr        -- ^ source array
                   -> CmmExpr        -- ^ offset in source array
                   -> CmmExpr        -- ^ destination array
                   -> CmmExpr        -- ^ offset in destination array
                   -> WordOff        -- ^ number of elements to copy
                   -> FCode ()
emitCopySmallArray copy src0 src_off dst0 dst_off n = do
    dflags <- getDynFlags

    -- Passed as arguments (be careful)
    src     <- assignTempE src0
    dst     <- assignTempE dst0

    -- Set the dirty bit in the header.
    emit (setInfo dst (CmmLit (CmmLabel mkSMAP_DIRTY_infoLabel)))

    dst_p <- assignTempE $ cmmOffsetExprW dflags
             (cmmOffsetB dflags dst (smallArrPtrsHdrSize dflags)) dst_off
    src_p <- assignTempE $ cmmOffsetExprW dflags
             (cmmOffsetB dflags src (smallArrPtrsHdrSize dflags)) src_off
    let bytes = wordsToBytes dflags n

    copy src dst dst_p src_p bytes

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy. Allocates a new array and
-- initializes it from the source array.
emitCloneArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> WordOff
               -> FCode ()
emitCloneArray info_p res_r src src_off n = do
    dflags <- getDynFlags

    let info_ptr = mkLblExpr info_p
        rep = arrPtrsRep dflags n

    tickyAllocPrim (mkIntExpr dflags (arrPtrsHdrSize dflags))
        (mkIntExpr dflags (nonHdrSize dflags rep))
        (zeroExpr dflags)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr curCCS
                     [ (mkIntExpr dflags n,
                        hdr_size + oFFSET_StgMutArrPtrs_ptrs dflags)
                     , (mkIntExpr dflags (nonHdrSizeW rep),
                        hdr_size + oFFSET_StgMutArrPtrs_size dflags)
                     ]

    arr <- CmmLocal `fmap` newTemp (bWord dflags)
    emit $ mkAssign arr base

    dst_p <- assignTempE $ cmmOffsetB dflags (CmmReg arr)
             (arrPtrsHdrSize dflags)
    src_p <- assignTempE $ cmmOffsetExprW dflags src
             (cmmAddWord dflags
              (mkIntExpr dflags (arrPtrsHdrSizeW dflags)) src_off)

    emitMemcpyCall dst_p src_p (mkIntExpr dflags (wordsToBytes dflags n))
        (wORD_SIZE dflags)

    emit $ mkAssign (CmmLocal res_r) (CmmReg arr)

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy. Allocates a new array and
-- initializes it from the source array.
emitCloneSmallArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> WordOff
                    -> FCode ()
emitCloneSmallArray info_p res_r src src_off n = do
    dflags <- getDynFlags

    let info_ptr = mkLblExpr info_p
        rep = smallArrPtrsRep n

    tickyAllocPrim (mkIntExpr dflags (smallArrPtrsHdrSize dflags))
        (mkIntExpr dflags (nonHdrSize dflags rep))
        (zeroExpr dflags)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr curCCS
                     [ (mkIntExpr dflags n,
                        hdr_size + oFFSET_StgSmallMutArrPtrs_ptrs dflags)
                     ]

    arr <- CmmLocal `fmap` newTemp (bWord dflags)
    emit $ mkAssign arr base

    dst_p <- assignTempE $ cmmOffsetB dflags (CmmReg arr)
             (smallArrPtrsHdrSize dflags)
    src_p <- assignTempE $ cmmOffsetExprW dflags src
             (cmmAddWord dflags
              (mkIntExpr dflags (smallArrPtrsHdrSizeW dflags)) src_off)

    emitMemcpyCall dst_p src_p (mkIntExpr dflags (wordsToBytes dflags n))
        (wORD_SIZE dflags)

    emit $ mkAssign (CmmLocal res_r) (CmmReg arr)

-- | Takes and offset in the destination array, the base address of
-- the card table, and the number of elements affected (*not* the
-- number of cards). The number of elements may not be zero.
-- Marks the relevant cards as dirty.
emitSetCards :: CmmExpr -> CmmExpr -> WordOff -> FCode ()
emitSetCards dst_start dst_cards_start n = do
    dflags <- getDynFlags
    start_card <- assignTempE $ cardCmm dflags dst_start
    let end_card = cardCmm dflags
                   (cmmSubWord dflags
                    (cmmAddWord dflags dst_start (mkIntExpr dflags n))
                    (mkIntExpr dflags 1))
    emitMemsetCall (cmmAddWord dflags dst_cards_start start_card)
        (mkIntExpr dflags 1)
        (cmmAddWord dflags (cmmSubWord dflags end_card start_card) (mkIntExpr dflags 1))
        1 -- no alignment (1 byte)

-- Convert an element index to a card index
cardCmm :: DynFlags -> CmmExpr -> CmmExpr
cardCmm dflags i =
    cmmUShrWord dflags i (mkIntExpr dflags (mUT_ARR_PTRS_CARD_BITS dflags))

------------------------------------------------------------------------------
-- SmallArray PrimOp implementations

doReadSmallPtrArrayOp :: LocalReg
                      -> CmmExpr
                      -> CmmExpr
                      -> FCode ()
doReadSmallPtrArrayOp res addr idx = do
    dflags <- getDynFlags
    mkBasicIndexedRead (smallArrPtrsHdrSize dflags) Nothing (gcWord dflags) res addr
        (gcWord dflags) idx

doWriteSmallPtrArrayOp :: CmmExpr
                       -> CmmExpr
                       -> CmmExpr
                       -> FCode ()
doWriteSmallPtrArrayOp addr idx val = do
    dflags <- getDynFlags
    let ty = cmmExprType dflags val
    mkBasicIndexedWrite (smallArrPtrsHdrSize dflags) Nothing addr ty idx val
    emit (setInfo addr (CmmLit (CmmLabel mkSMAP_DIRTY_infoLabel)))

------------------------------------------------------------------------------
-- Atomic read-modify-write

-- | Emit an atomic modification to a byte array element. The result
-- reg contains that previous value of the element. Implies a full
-- memory barrier.
doAtomicRMW :: LocalReg      -- ^ Result reg
            -> AtomicMachOp  -- ^ Atomic op (e.g. add)
            -> CmmExpr       -- ^ MutableByteArray#
            -> CmmExpr       -- ^ Index
            -> CmmType       -- ^ Type of element by which we are indexing
            -> CmmExpr       -- ^ Op argument (e.g. amount to add)
            -> FCode ()
doAtomicRMW res amop mba idx idx_ty n = do
    dflags <- getDynFlags
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr dflags (arrWordsHdrSize dflags)
                width mba idx
    emitPrimCall
        [ res ]
        (MO_AtomicRMW width amop)
        [ addr, n ]

-- | Emit an atomic read to a byte array that acts as a memory barrier.
doAtomicReadByteArray
    :: LocalReg  -- ^ Result reg
    -> CmmExpr   -- ^ MutableByteArray#
    -> CmmExpr   -- ^ Index
    -> CmmType   -- ^ Type of element by which we are indexing
    -> FCode ()
doAtomicReadByteArray res mba idx idx_ty = do
    dflags <- getDynFlags
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr dflags (arrWordsHdrSize dflags)
                width mba idx
    emitPrimCall
        [ res ]
        (MO_AtomicRead width)
        [ addr ]

-- | Emit an atomic write to a byte array that acts as a memory barrier.
doAtomicWriteByteArray
    :: CmmExpr   -- ^ MutableByteArray#
    -> CmmExpr   -- ^ Index
    -> CmmType   -- ^ Type of element by which we are indexing
    -> CmmExpr   -- ^ Value to write
    -> FCode ()
doAtomicWriteByteArray mba idx idx_ty val = do
    dflags <- getDynFlags
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr dflags (arrWordsHdrSize dflags)
                width mba idx
    emitPrimCall
        [ {- no results -} ]
        (MO_AtomicWrite width)
        [ addr, val ]

doCasByteArray
    :: LocalReg  -- ^ Result reg
    -> CmmExpr   -- ^ MutableByteArray#
    -> CmmExpr   -- ^ Index
    -> CmmType   -- ^ Type of element by which we are indexing
    -> CmmExpr   -- ^ Old value
    -> CmmExpr   -- ^ New value
    -> FCode ()
doCasByteArray res mba idx idx_ty old new = do
    dflags <- getDynFlags
    let width = (typeWidth idx_ty)
        addr = cmmIndexOffExpr dflags (arrWordsHdrSize dflags)
               width mba idx
    emitPrimCall
        [ res ]
        (MO_Cmpxchg width)
        [ addr, old, new ]

------------------------------------------------------------------------------
-- Helpers for emitting function calls

-- | Emit a call to @memcpy@.
emitMemcpyCall :: CmmExpr -> CmmExpr -> CmmExpr -> Int -> FCode ()
emitMemcpyCall dst src n align = do
    emitPrimCall
        [ {-no results-} ]
        (MO_Memcpy align)
        [ dst, src, n ]

-- | Emit a call to @memmove@.
emitMemmoveCall :: CmmExpr -> CmmExpr -> CmmExpr -> Int -> FCode ()
emitMemmoveCall dst src n align = do
    emitPrimCall
        [ {- no results -} ]
        (MO_Memmove align)
        [ dst, src, n ]

-- | Emit a call to @memset@.  The second argument must fit inside an
-- unsigned char.
emitMemsetCall :: CmmExpr -> CmmExpr -> CmmExpr -> Int -> FCode ()
emitMemsetCall dst c n align = do
    emitPrimCall
        [ {- no results -} ]
        (MO_Memset align)
        [ dst, c, n ]

emitMemcmpCall :: LocalReg -> CmmExpr -> CmmExpr -> CmmExpr -> Int -> FCode ()
emitMemcmpCall res ptr1 ptr2 n align = do
    -- 'MO_Memcmp' is assumed to return an 32bit 'CInt' because all
    -- code-gens currently call out to the @memcmp(3)@ C function.
    -- This was easier than moving the sign-extensions into
    -- all the code-gens.
    dflags <- getDynFlags
    let is32Bit = typeWidth (localRegType res) == W32

    cres <- if is32Bit
              then return res
              else newTemp b32

    emitPrimCall
        [ cres ]
        (MO_Memcmp align)
        [ ptr1, ptr2, n ]

    unless is32Bit $ do
      emit $ mkAssign (CmmLocal res)
                      (CmmMachOp
                         (mo_s_32ToWord dflags)
                         [(CmmReg (CmmLocal cres))])

emitBSwapCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitBSwapCall res x width = do
    emitPrimCall
        [ res ]
        (MO_BSwap width)
        [ x ]

emitPopCntCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitPopCntCall res x width = do
    emitPrimCall
        [ res ]
        (MO_PopCnt width)
        [ x ]

emitClzCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitClzCall res x width = do
    emitPrimCall
        [ res ]
        (MO_Clz width)
        [ x ]

emitCtzCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitCtzCall res x width = do
    emitPrimCall
        [ res ]
        (MO_Ctz width)
        [ x ]
