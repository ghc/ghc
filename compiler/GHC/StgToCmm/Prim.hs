{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#if __GLASGOW_HASKELL__ <= 808
-- GHC 8.10 deprecates this flag, but GHC 8.8 needs it
-- emitPrimOp is quite large
{-# OPTIONS_GHC -fmax-pmcheck-iterations=4000000 #-}
#endif

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

----------------------------------------------------------------------------
--
-- Stg to C--: primitive operations
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Prim (
   cgOpApp,
   shouldInlinePrimOp
 ) where

#include "HsVersions.h"

import GHC.Prelude hiding ((<*>))

import GHC.StgToCmm.Layout
import GHC.StgToCmm.Foreign
import GHC.StgToCmm.Env
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Ticky
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Prof ( costCentreFrom )

import GHC.Driver.Session
import GHC.Platform
import GHC.Types.Basic
import GHC.Cmm.BlockId
import GHC.Cmm.Graph
import GHC.Stg.Syntax
import GHC.Cmm
import GHC.Unit         ( rtsUnitId )
import GHC.Core.Type    ( Type, tyConAppTyCon )
import GHC.Core.TyCon
import GHC.Cmm.CLabel
import GHC.Cmm.Utils
import GHC.Builtin.PrimOps
import GHC.Runtime.Heap.Layout
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Misc
import Data.Maybe

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
cgOpApp (StgFCallOp fcall ty) stg_args res_ty
  = cgForeignCall fcall ty stg_args res_ty
      -- Note [Foreign call results]

cgOpApp (StgPrimOp primop) args res_ty = do
    dflags <- getDynFlags
    cmm_args <- getNonVoidArgAmodes args
    case emitPrimOp dflags primop cmm_args of
        PrimopCmmEmit_External -> do  -- out-of-line
          let fun = CmmLit (CmmLabel (mkRtsPrimOpLabel primop))
          emitCall (NativeNodeCall, NativeReturn) fun cmm_args

        PrimopCmmEmit_Raw f -> do
          exprs <- f res_ty
          emitReturn exprs

        PrimopCmmEmit_IntoRegs f  -- inline
          | ReturnsPrim VoidRep <- result_info
          -> do f []
                emitReturn []

          | ReturnsPrim rep <- result_info
          -> do platform <- getPlatform
                res <- newTemp (primRepCmmType platform rep)
                f [res]
                emitReturn [CmmReg (CmmLocal res)]

          | ReturnsAlg tycon <- result_info, isUnboxedTupleTyCon tycon
          -> do (regs, _hints) <- newUnboxedTupleRegs res_ty
                f regs
                emitReturn (map (CmmReg . CmmLocal) regs)

          | otherwise -> panic "cgOpApp"
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

------------------------------------------------------------------------
--      Emitting code for a primop
------------------------------------------------------------------------

shouldInlinePrimOp :: DynFlags -> PrimOp -> [CmmExpr] -> Bool
shouldInlinePrimOp dflags op args = case emitPrimOp dflags op args of
  PrimopCmmEmit_External -> False
  PrimopCmmEmit_IntoRegs _ -> True
  PrimopCmmEmit_Raw _ -> True

-- TODO: Several primop implementations (e.g. 'doNewByteArrayOp') use
-- ByteOff (or some other fixed width signed type) to represent
-- array sizes or indices. This means that these will overflow for
-- large enough sizes.

-- TODO: Several primops, such as 'copyArray#', only have an inline
-- implementation (below) but could possibly have both an inline
-- implementation and an out-of-line implementation, just like
-- 'newArray#'. This would lower the amount of code generated,
-- hopefully without a performance impact (needs to be measured).

-- | The big function handling all the primops.
--
-- In the simple case, there is just one implementation, and we emit that.
--
-- In more complex cases, there is a foreign call (out of line) fallback. This
-- might happen e.g. if there's enough static information, such as statically
-- know arguments.
emitPrimOp
  :: DynFlags
  -> PrimOp            -- ^ The primop
  -> [CmmExpr]         -- ^ The primop arguments
  -> PrimopCmmEmit
emitPrimOp dflags = \case
  NewByteArrayOp_Char -> \case
    [(CmmLit (CmmInt n w))]
      | asUnsigned w n <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone  $ \ [res] -> doNewByteArrayOp res (fromInteger n)
    _ -> PrimopCmmEmit_External

  NewArrayOp -> \case
    [(CmmLit (CmmInt n w)), init]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \[res] -> doNewArrayOp res (arrPtrsRep dflags (fromInteger n)) mkMAP_DIRTY_infoLabel
        [ (mkIntExpr platform (fromInteger n),
           fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags)
        , (mkIntExpr platform (nonHdrSizeW (arrPtrsRep dflags (fromInteger n))),
           fixedHdrSize dflags + oFFSET_StgMutArrPtrs_size dflags)
        ]
        (fromInteger n) init
    _ -> PrimopCmmEmit_External

  CopyArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopyArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CopyMutableArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopyMutableArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CopyArrayArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopyArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CopyMutableArrayArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopyMutableArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CloneArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneArray mkMAP_FROZEN_CLEAN_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CloneMutableArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneArray mkMAP_DIRTY_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  FreezeArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneArray mkMAP_FROZEN_CLEAN_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  ThawArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneArray mkMAP_DIRTY_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  NewSmallArrayOp -> \case
    [(CmmLit (CmmInt n w)), init]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] ->
        doNewArrayOp res (smallArrPtrsRep (fromInteger n)) mkSMAP_DIRTY_infoLabel
        [ (mkIntExpr platform (fromInteger n),
           fixedHdrSize dflags + oFFSET_StgSmallMutArrPtrs_ptrs dflags)
        ]
        (fromInteger n) init
    _ -> PrimopCmmEmit_External

  CopySmallArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopySmallArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CopySmallMutableArrayOp -> \case
    [src, src_off, dst, dst_off, (CmmLit (CmmInt n _))] ->
      opAllDone $ \ [] -> doCopySmallMutableArrayOp src src_off dst dst_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CloneSmallArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneSmallArray mkSMAP_FROZEN_CLEAN_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  CloneSmallMutableArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneSmallArray mkSMAP_DIRTY_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  FreezeSmallArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneSmallArray mkSMAP_FROZEN_CLEAN_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

  ThawSmallArrayOp -> \case
    [src, src_off, (CmmLit (CmmInt n w))]
      | wordsToBytes platform (asUnsigned w n) <= fromIntegral (maxInlineAllocSize dflags)
      -> opAllDone $ \ [res] -> emitCloneSmallArray mkSMAP_DIRTY_infoLabel res src src_off (fromInteger n)
    _ -> PrimopCmmEmit_External

-- First we handle various awkward cases specially.

  ParOp -> \[arg] -> opAllDone $ \[res] -> do
    -- for now, just implement this in a C function
    -- later, we might want to inline it.
    emitCCall
        [(res,NoHint)]
        (CmmLit (CmmLabel (mkForeignLabel (fsLit "newSpark") Nothing ForeignLabelInExternalPackage IsFunction)))
        [(baseExpr, AddrHint), (arg,AddrHint)]

  SparkOp -> \[arg] -> opAllDone $ \[res] -> do
    -- returns the value of arg in res.  We're going to therefore
    -- refer to arg twice (once to pass to newSpark(), and once to
    -- assign to res), so put it in a temporary.
    tmp <- assignTemp arg
    tmp2 <- newTemp (bWord platform)
    emitCCall
        [(tmp2,NoHint)]
        (CmmLit (CmmLabel (mkForeignLabel (fsLit "newSpark") Nothing ForeignLabelInExternalPackage IsFunction)))
        [(baseExpr, AddrHint), ((CmmReg (CmmLocal tmp)), AddrHint)]
    emitAssign (CmmLocal res) (CmmReg (CmmLocal tmp))

  GetCCSOfOp -> \[arg] -> opAllDone $ \[res] -> do
    let
      val
       | gopt Opt_SccProfilingOn dflags = costCentreFrom dflags (cmmUntag dflags arg)
       | otherwise                      = CmmLit (zeroCLit platform)
    emitAssign (CmmLocal res) val

  GetCurrentCCSOp -> \[_] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) cccsExpr

  MyThreadIdOp -> \[] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) currentTSOExpr

  ReadMutVarOp -> \[mutv] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) (cmmLoadIndexW platform mutv (fixedHdrSizeW dflags) (gcWord platform))

  WriteMutVarOp -> \[mutv, var] -> opAllDone $ \res@[] -> do
    old_val <- CmmLocal <$> newTemp (cmmExprType platform var)
    emitAssign old_val (cmmLoadIndexW platform mutv (fixedHdrSizeW dflags) (gcWord platform))

    -- Without this write barrier, other CPUs may see this pointer before
    -- the writes for the closure it points to have occurred.
    -- Note that this also must come after we read the old value to ensure
    -- that the read of old_val comes before another core's write to the
    -- MutVar's value.
    emitPrimCall res MO_WriteBarrier []
    emitStore (cmmOffsetW platform mutv (fixedHdrSizeW dflags)) var
    emitCCall
            [{-no results-}]
            (CmmLit (CmmLabel mkDirty_MUT_VAR_Label))
            [(baseExpr, AddrHint), (mutv, AddrHint), (CmmReg old_val, AddrHint)]

--  #define sizzeofByteArrayzh(r,a) \
--     r = ((StgArrBytes *)(a))->bytes
  SizeofByteArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ mkAssign (CmmLocal res) (cmmLoadIndexW platform arg (fixedHdrSizeW dflags) (bWord platform))

--  #define sizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrBytes *)(a))->bytes
  SizeofMutableByteArrayOp -> emitPrimOp dflags SizeofByteArrayOp

--  #define getSizzeofMutableByteArrayzh(r,a) \
--      r = ((StgArrBytes *)(a))->bytes
  GetSizeofMutableByteArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) (cmmLoadIndexW platform arg (fixedHdrSizeW dflags) (bWord platform))


--  #define touchzh(o)                  /* nothing */
  TouchOp -> \args@[_] -> opAllDone $ \res@[] -> do
    emitPrimCall res MO_Touch args

--  #define byteArrayContentszh(r,a) r = BYTE_ARR_CTS(a)
  ByteArrayContents_Char -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) (cmmOffsetB platform arg (arrWordsHdrSize dflags))

--  #define stableNameToIntzh(r,s)   (r = ((StgStableName *)s)->sn)
  StableNameToIntOp -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) (cmmLoadIndexW platform arg (fixedHdrSizeW dflags) (bWord platform))

  ReallyUnsafePtrEqualityOp -> \[arg1, arg2] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) (CmmMachOp (mo_wordEq platform) [arg1,arg2])

--  #define addrToHValuezh(r,a) r=(P_)a
  AddrToAnyOp -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) arg

--  #define hvalueToAddrzh(r, a) r=(W_)a
  AnyToAddrOp -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) arg

{- Freezing arrays-of-ptrs requires changing an info table, for the
   benefit of the generational collector.  It needs to scavenge mutable
   objects, even if they are in old space.  When they become immutable,
   they can be removed from this scavenge list.  -}

--  #define unsafeFreezzeArrayzh(r,a)
--      {
--        SET_INFO((StgClosure *)a,&stg_MUT_ARR_PTRS_FROZEN_DIRTY_info);
--        r = a;
--      }
  UnsafeFreezeArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ catAGraphs
      [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_DIRTY_infoLabel)),
        mkAssign (CmmLocal res) arg ]
  UnsafeFreezeArrayArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ catAGraphs
      [ setInfo arg (CmmLit (CmmLabel mkMAP_FROZEN_DIRTY_infoLabel)),
        mkAssign (CmmLocal res) arg ]
  UnsafeFreezeSmallArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ catAGraphs
      [ setInfo arg (CmmLit (CmmLabel mkSMAP_FROZEN_DIRTY_infoLabel)),
        mkAssign (CmmLocal res) arg ]

--  #define unsafeFreezzeByteArrayzh(r,a)       r=(a)
  UnsafeFreezeByteArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emitAssign (CmmLocal res) arg

-- Reading/writing pointer arrays

  ReadArrayOp -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  IndexArrayOp -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  WriteArrayOp -> \[obj, ix, v] -> opAllDone $ \[] -> do
    doWritePtrArrayOp obj ix v

  IndexArrayArrayOp_ByteArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  IndexArrayArrayOp_ArrayArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  ReadArrayArrayOp_ByteArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  ReadArrayArrayOp_MutableByteArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  ReadArrayArrayOp_ArrayArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  ReadArrayArrayOp_MutableArrayArray -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadPtrArrayOp res obj ix
  WriteArrayArrayOp_ByteArray -> \[obj,ix,v] -> opAllDone $ \[] -> do
    doWritePtrArrayOp obj ix v
  WriteArrayArrayOp_MutableByteArray -> \[obj,ix,v] -> opAllDone $ \[] -> do
    doWritePtrArrayOp obj ix v
  WriteArrayArrayOp_ArrayArray -> \[obj,ix,v] -> opAllDone $ \[] -> do
    doWritePtrArrayOp obj ix v
  WriteArrayArrayOp_MutableArrayArray -> \[obj,ix,v] -> opAllDone $ \[] -> do
    doWritePtrArrayOp obj ix v

  ReadSmallArrayOp -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadSmallPtrArrayOp res obj ix
  IndexSmallArrayOp -> \[obj, ix] -> opAllDone $ \[res] -> do
    doReadSmallPtrArrayOp res obj ix
  WriteSmallArrayOp -> \[obj,ix,v] -> opAllDone $ \[] -> do
    doWriteSmallPtrArrayOp obj ix v

-- Getting the size of pointer arrays

  SizeofArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ mkAssign (CmmLocal res) (cmmLoadIndexW platform arg
      (fixedHdrSizeW dflags + bytesToWordsRoundUp platform (oFFSET_StgMutArrPtrs_ptrs dflags))
        (bWord platform))
  SizeofMutableArrayOp -> emitPrimOp dflags SizeofArrayOp
  SizeofArrayArrayOp -> emitPrimOp dflags SizeofArrayOp
  SizeofMutableArrayArrayOp -> emitPrimOp dflags SizeofArrayOp
  SizeofSmallArrayOp -> \[arg] -> opAllDone $ \[res] -> do
    emit $ mkAssign (CmmLocal res)
     (cmmLoadIndexW platform arg
     (fixedHdrSizeW dflags + bytesToWordsRoundUp platform (oFFSET_StgSmallMutArrPtrs_ptrs dflags))
        (bWord platform))

  SizeofSmallMutableArrayOp -> emitPrimOp dflags SizeofSmallArrayOp
  GetSizeofSmallMutableArrayOp -> emitPrimOp dflags SizeofSmallArrayOp

-- IndexXXXoffAddr

  IndexOffAddrOp_Char -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_8ToWord platform)) b8 res args
  IndexOffAddrOp_WideChar -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_32ToWord platform)) b32 res args
  IndexOffAddrOp_Int -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  IndexOffAddrOp_Word -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  IndexOffAddrOp_Addr -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  IndexOffAddrOp_Float -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing f32 res args
  IndexOffAddrOp_Double -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing f64 res args
  IndexOffAddrOp_StablePtr -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  IndexOffAddrOp_Int8 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_8ToWord platform)) b8  res args
  IndexOffAddrOp_Int16 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_16ToWord platform)) b16 res args
  IndexOffAddrOp_Int32 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_32ToWord platform)) b32 res args
  IndexOffAddrOp_Int64 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing b64 res args
  IndexOffAddrOp_Word8 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_8ToWord platform)) b8  res args
  IndexOffAddrOp_Word16 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_16ToWord platform)) b16 res args
  IndexOffAddrOp_Word32 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_32ToWord platform)) b32 res args
  IndexOffAddrOp_Word64 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing b64 res args

-- ReadXXXoffAddr, which are identical, for our purposes, to IndexXXXoffAddr.

  ReadOffAddrOp_Char -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_8ToWord platform)) b8 res args
  ReadOffAddrOp_WideChar -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_32ToWord platform)) b32 res args
  ReadOffAddrOp_Int -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  ReadOffAddrOp_Word -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  ReadOffAddrOp_Addr -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  ReadOffAddrOp_Float -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing f32 res args
  ReadOffAddrOp_Double -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing f64 res args
  ReadOffAddrOp_StablePtr -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing (bWord platform) res args
  ReadOffAddrOp_Int8 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_8ToWord platform)) b8  res args
  ReadOffAddrOp_Int16 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_16ToWord platform)) b16 res args
  ReadOffAddrOp_Int32 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_s_32ToWord platform)) b32 res args
  ReadOffAddrOp_Int64 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing b64 res args
  ReadOffAddrOp_Word8 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_8ToWord platform)) b8  res args
  ReadOffAddrOp_Word16 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_16ToWord platform)) b16 res args
  ReadOffAddrOp_Word32 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   (Just (mo_u_32ToWord platform)) b32 res args
  ReadOffAddrOp_Word64 -> \args -> opAllDone $ \res -> do
    doIndexOffAddrOp   Nothing b64 res args

-- IndexXXXArray

  IndexByteArrayOp_Char -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_8ToWord platform)) b8 res args
  IndexByteArrayOp_WideChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_32ToWord platform)) b32 res args
  IndexByteArrayOp_Int -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  IndexByteArrayOp_Word -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  IndexByteArrayOp_Addr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  IndexByteArrayOp_Float -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing f32 res args
  IndexByteArrayOp_Double -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing f64 res args
  IndexByteArrayOp_StablePtr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  IndexByteArrayOp_Int8 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_8ToWord platform)) b8  res args
  IndexByteArrayOp_Int16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_16ToWord platform)) b16  res args
  IndexByteArrayOp_Int32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_32ToWord platform)) b32  res args
  IndexByteArrayOp_Int64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing b64  res args
  IndexByteArrayOp_Word8 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_8ToWord platform)) b8  res args
  IndexByteArrayOp_Word16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_16ToWord platform)) b16  res args
  IndexByteArrayOp_Word32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_32ToWord platform)) b32  res args
  IndexByteArrayOp_Word64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing b64  res args

-- ReadXXXArray, identical to IndexXXXArray.

  ReadByteArrayOp_Char -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_8ToWord platform)) b8 res args
  ReadByteArrayOp_WideChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_32ToWord platform)) b32 res args
  ReadByteArrayOp_Int -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  ReadByteArrayOp_Word -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  ReadByteArrayOp_Addr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  ReadByteArrayOp_Float -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing f32 res args
  ReadByteArrayOp_Double -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing f64 res args
  ReadByteArrayOp_StablePtr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing (bWord platform) res args
  ReadByteArrayOp_Int8 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_8ToWord platform)) b8  res args
  ReadByteArrayOp_Int16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_16ToWord platform)) b16  res args
  ReadByteArrayOp_Int32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_s_32ToWord platform)) b32  res args
  ReadByteArrayOp_Int64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing b64  res args
  ReadByteArrayOp_Word8 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_8ToWord platform)) b8  res args
  ReadByteArrayOp_Word16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_16ToWord platform)) b16  res args
  ReadByteArrayOp_Word32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   (Just (mo_u_32ToWord platform)) b32  res args
  ReadByteArrayOp_Word64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOp   Nothing b64  res args

-- IndexWord8ArrayAsXXX

  IndexByteArrayOp_Word8AsChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_8ToWord platform)) b8 b8 res args
  IndexByteArrayOp_Word8AsWideChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_32ToWord platform)) b32 b8 res args
  IndexByteArrayOp_Word8AsInt -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  IndexByteArrayOp_Word8AsWord -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  IndexByteArrayOp_Word8AsAddr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  IndexByteArrayOp_Word8AsFloat -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing f32 b8 res args
  IndexByteArrayOp_Word8AsDouble -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing f64 b8 res args
  IndexByteArrayOp_Word8AsStablePtr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  IndexByteArrayOp_Word8AsInt16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_s_16ToWord platform)) b16 b8 res args
  IndexByteArrayOp_Word8AsInt32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_s_32ToWord platform)) b32 b8 res args
  IndexByteArrayOp_Word8AsInt64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing b64 b8 res args
  IndexByteArrayOp_Word8AsWord16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_16ToWord platform)) b16 b8 res args
  IndexByteArrayOp_Word8AsWord32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_32ToWord platform)) b32 b8 res args
  IndexByteArrayOp_Word8AsWord64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing b64 b8 res args

-- ReadInt8ArrayAsXXX, identical to IndexInt8ArrayAsXXX

  ReadByteArrayOp_Word8AsChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_8ToWord platform)) b8 b8 res args
  ReadByteArrayOp_Word8AsWideChar -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_32ToWord platform)) b32 b8 res args
  ReadByteArrayOp_Word8AsInt -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  ReadByteArrayOp_Word8AsWord -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  ReadByteArrayOp_Word8AsAddr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  ReadByteArrayOp_Word8AsFloat -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing f32 b8 res args
  ReadByteArrayOp_Word8AsDouble -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing f64 b8 res args
  ReadByteArrayOp_Word8AsStablePtr -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing (bWord platform) b8 res args
  ReadByteArrayOp_Word8AsInt16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_s_16ToWord platform)) b16 b8 res args
  ReadByteArrayOp_Word8AsInt32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_s_32ToWord platform)) b32 b8 res args
  ReadByteArrayOp_Word8AsInt64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing b64 b8 res args
  ReadByteArrayOp_Word8AsWord16 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_16ToWord platform)) b16 b8 res args
  ReadByteArrayOp_Word8AsWord32 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   (Just (mo_u_32ToWord platform)) b32 b8 res args
  ReadByteArrayOp_Word8AsWord64 -> \args -> opAllDone $ \res -> do
    doIndexByteArrayOpAs   Nothing b64 b8 res args

-- WriteXXXoffAddr

  WriteOffAddrOp_Char -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo8 platform))  b8 res args
  WriteOffAddrOp_WideChar -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo32 platform)) b32 res args
  WriteOffAddrOp_Int -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing (bWord platform) res args
  WriteOffAddrOp_Word -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing (bWord platform) res args
  WriteOffAddrOp_Addr -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing (bWord platform) res args
  WriteOffAddrOp_Float -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing f32 res args
  WriteOffAddrOp_Double -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing f64 res args
  WriteOffAddrOp_StablePtr -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing (bWord platform) res args
  WriteOffAddrOp_Int8 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo8 platform))  b8 res args
  WriteOffAddrOp_Int16 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo16 platform)) b16 res args
  WriteOffAddrOp_Int32 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo32 platform)) b32 res args
  WriteOffAddrOp_Int64 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing b64 res args
  WriteOffAddrOp_Word8 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo8 platform))  b8 res args
  WriteOffAddrOp_Word16 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo16 platform)) b16 res args
  WriteOffAddrOp_Word32 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp (Just (mo_WordTo32 platform)) b32 res args
  WriteOffAddrOp_Word64 -> \args -> opAllDone $ \res -> do
    doWriteOffAddrOp Nothing b64 res args

-- WriteXXXArray

  WriteByteArrayOp_Char -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo8 platform))  b8 res args
  WriteByteArrayOp_WideChar -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b32 res args
  WriteByteArrayOp_Int -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing (bWord platform) res args
  WriteByteArrayOp_Word -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing (bWord platform) res args
  WriteByteArrayOp_Addr -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing (bWord platform) res args
  WriteByteArrayOp_Float -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing f32 res args
  WriteByteArrayOp_Double -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing f64 res args
  WriteByteArrayOp_StablePtr -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing (bWord platform) res args
  WriteByteArrayOp_Int8 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo8 platform))  b8 res args
  WriteByteArrayOp_Int16 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo16 platform)) b16 res args
  WriteByteArrayOp_Int32 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b32 res args
  WriteByteArrayOp_Int64 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b64 res args
  WriteByteArrayOp_Word8 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo8 platform))  b8  res args
  WriteByteArrayOp_Word16 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo16 platform)) b16 res args
  WriteByteArrayOp_Word32 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b32 res args
  WriteByteArrayOp_Word64 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b64 res args

-- WriteInt8ArrayAsXXX

  WriteByteArrayOp_Word8AsChar -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo8 platform))  b8 res args
  WriteByteArrayOp_Word8AsWideChar -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b8 res args
  WriteByteArrayOp_Word8AsInt -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsWord -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsAddr -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsFloat -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsDouble -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsStablePtr -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsInt16 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo16 platform)) b8 res args
  WriteByteArrayOp_Word8AsInt32 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b8 res args
  WriteByteArrayOp_Word8AsInt64 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args
  WriteByteArrayOp_Word8AsWord16 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo16 platform)) b8 res args
  WriteByteArrayOp_Word8AsWord32 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp (Just (mo_WordTo32 platform)) b8 res args
  WriteByteArrayOp_Word8AsWord64 -> \args -> opAllDone $ \res -> do
    doWriteByteArrayOp Nothing b8 res args

-- Copying and setting byte arrays
  CopyByteArrayOp -> \[src,src_off,dst,dst_off,n] -> opAllDone $ \[] -> do
    doCopyByteArrayOp src src_off dst dst_off n
  CopyMutableByteArrayOp -> \[src,src_off,dst,dst_off,n] -> opAllDone $ \[] -> do
    doCopyMutableByteArrayOp src src_off dst dst_off n
  CopyByteArrayToAddrOp -> \[src,src_off,dst,n] -> opAllDone $ \[] -> do
    doCopyByteArrayToAddrOp src src_off dst n
  CopyMutableByteArrayToAddrOp -> \[src,src_off,dst,n] -> opAllDone $ \[] -> do
    doCopyMutableByteArrayToAddrOp src src_off dst n
  CopyAddrToByteArrayOp -> \[src,dst,dst_off,n] -> opAllDone $ \[] -> do
    doCopyAddrToByteArrayOp src dst dst_off n
  SetByteArrayOp -> \[ba,off,len,c] -> opAllDone $ \[] -> do
    doSetByteArrayOp ba off len c

-- Comparing byte arrays
  CompareByteArraysOp -> \[ba1,ba1_off,ba2,ba2_off,n] -> opAllDone $ \[res] -> do
    doCompareByteArraysOp res ba1 ba1_off ba2 ba2_off n

  BSwap16Op -> \[w] -> opAllDone $ \[res] -> do
    emitBSwapCall res w W16
  BSwap32Op -> \[w] -> opAllDone $ \[res] -> do
    emitBSwapCall res w W32
  BSwap64Op -> \[w] -> opAllDone $ \[res] -> do
    emitBSwapCall res w W64
  BSwapOp -> \[w] -> opAllDone $ \[res] -> do
    emitBSwapCall res w (wordWidth platform)

  BRev8Op -> \[w] -> opAllDone $ \[res] -> do
    emitBRevCall res w W8
  BRev16Op -> \[w] -> opAllDone $ \[res] -> do
    emitBRevCall res w W16
  BRev32Op -> \[w] -> opAllDone $ \[res] -> do
    emitBRevCall res w W32
  BRev64Op -> \[w] -> opAllDone $ \[res] -> do
    emitBRevCall res w W64
  BRevOp -> \[w] -> opAllDone $ \[res] -> do
    emitBRevCall res w (wordWidth platform)

-- Population count
  PopCnt8Op -> \[w] -> opAllDone $ \[res] -> do
    emitPopCntCall res w W8
  PopCnt16Op -> \[w] -> opAllDone $ \[res] -> do
    emitPopCntCall res w W16
  PopCnt32Op -> \[w] -> opAllDone $ \[res] -> do
    emitPopCntCall res w W32
  PopCnt64Op -> \[w] -> opAllDone $ \[res] -> do
    emitPopCntCall res w W64
  PopCntOp -> \[w] -> opAllDone $ \[res] -> do
    emitPopCntCall res w (wordWidth platform)

-- Parallel bit deposit
  Pdep8Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPdepCall res src mask W8
  Pdep16Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPdepCall res src mask W16
  Pdep32Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPdepCall res src mask W32
  Pdep64Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPdepCall res src mask W64
  PdepOp -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPdepCall res src mask (wordWidth platform)

-- Parallel bit extract
  Pext8Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPextCall res src mask W8
  Pext16Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPextCall res src mask W16
  Pext32Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPextCall res src mask W32
  Pext64Op -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPextCall res src mask W64
  PextOp -> \[src, mask] -> opAllDone $ \[res] -> do
    emitPextCall res src mask (wordWidth platform)

-- count leading zeros
  Clz8Op -> \[w] -> opAllDone $ \[res] -> do
    emitClzCall res w W8
  Clz16Op -> \[w] -> opAllDone $ \[res] -> do
    emitClzCall res w W16
  Clz32Op -> \[w] -> opAllDone $ \[res] -> do
    emitClzCall res w W32
  Clz64Op -> \[w] -> opAllDone $ \[res] -> do
    emitClzCall res w W64
  ClzOp -> \[w] -> opAllDone $ \[res] -> do
    emitClzCall res w (wordWidth platform)

-- count trailing zeros
  Ctz8Op -> \[w] -> opAllDone $ \[res] -> do
    emitCtzCall res w W8
  Ctz16Op -> \[w] -> opAllDone $ \[res] -> do
    emitCtzCall res w W16
  Ctz32Op -> \[w] -> opAllDone $ \[res] -> do
    emitCtzCall res w W32
  Ctz64Op -> \[w] -> opAllDone $ \[res] -> do
    emitCtzCall res w W64
  CtzOp -> \[w] -> opAllDone $ \[res] -> do
    emitCtzCall res w (wordWidth platform)

-- Unsigned int to floating point conversions
  Word2FloatOp -> \[w] -> opAllDone $ \[res] -> do
    emitPrimCall [res] (MO_UF_Conv W32) [w]
  Word2DoubleOp -> \[w] -> opAllDone $ \[res] -> do
    emitPrimCall [res] (MO_UF_Conv W64) [w]

-- SIMD primops
  (VecBroadcastOp vcat n w) -> \[e] -> opAllDone $ \[res] -> do
    checkVecCompatibility dflags vcat n w
    doVecPackOp (vecElemInjectCast platform vcat w) ty zeros (replicate n e) res
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

  (VecPackOp vcat n w) -> \es -> opAllDone $ \[res] -> do
    checkVecCompatibility dflags vcat n w
    when (es `lengthIsNot` n) $
        panic "emitPrimOp: VecPackOp has wrong number of arguments"
    doVecPackOp (vecElemInjectCast platform vcat w) ty zeros es res
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

  (VecUnpackOp vcat n w) -> \[arg] -> opAllDone $ \res -> do
    checkVecCompatibility dflags vcat n w
    when (res `lengthIsNot` n) $
        panic "emitPrimOp: VecUnpackOp has wrong number of results"
    doVecUnpackOp (vecElemProjectCast platform vcat w) ty arg res
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecInsertOp vcat n w) -> \[v,e,i] -> opAllDone $ \[res] -> do
    checkVecCompatibility dflags vcat n w
    doVecInsertOp (vecElemInjectCast platform vcat w) ty v e i res
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecIndexByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecReadByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecWriteByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doWriteByteArrayOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecIndexOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecReadOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecWriteOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doWriteOffAddrOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecVmmType vcat n w

  (VecIndexScalarByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOpAs Nothing vecty ty res0 args
   where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

  (VecReadScalarByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexByteArrayOpAs Nothing vecty ty res0 args
   where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

  (VecWriteScalarByteArrayOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doWriteByteArrayOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecCmmCat vcat w

  (VecIndexScalarOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOpAs Nothing vecty ty res0 args
   where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

  (VecReadScalarOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doIndexOffAddrOpAs Nothing vecty ty res0 args
   where
    vecty :: CmmType
    vecty = vecVmmType vcat n w

    ty :: CmmType
    ty = vecCmmCat vcat w

  (VecWriteScalarOffAddrOp vcat n w) -> \args -> opAllDone $ \res0 -> do
    checkVecCompatibility dflags vcat n w
    doWriteOffAddrOp Nothing ty res0 args
   where
    ty :: CmmType
    ty = vecCmmCat vcat w

-- Prefetch
  PrefetchByteArrayOp3         -> \args -> opAllDone $ \[] -> do
    doPrefetchByteArrayOp 3  args
  PrefetchMutableByteArrayOp3  -> \args -> opAllDone $ \[] -> do
    doPrefetchMutableByteArrayOp 3  args
  PrefetchAddrOp3              -> \args -> opAllDone $ \[] -> do
    doPrefetchAddrOp  3  args
  PrefetchValueOp3             -> \args -> opAllDone $ \[] -> do
    doPrefetchValueOp 3 args

  PrefetchByteArrayOp2         -> \args -> opAllDone $ \[] -> do
    doPrefetchByteArrayOp 2  args
  PrefetchMutableByteArrayOp2  -> \args -> opAllDone $ \[] -> do
    doPrefetchMutableByteArrayOp 2  args
  PrefetchAddrOp2              -> \args -> opAllDone $ \[] -> do
    doPrefetchAddrOp 2  args
  PrefetchValueOp2             -> \args -> opAllDone $ \[] -> do
    doPrefetchValueOp 2 args
  PrefetchByteArrayOp1         -> \args -> opAllDone $ \[] -> do
    doPrefetchByteArrayOp 1  args
  PrefetchMutableByteArrayOp1  -> \args -> opAllDone $ \[] -> do
    doPrefetchMutableByteArrayOp 1  args
  PrefetchAddrOp1              -> \args -> opAllDone $ \[] -> do
    doPrefetchAddrOp 1  args
  PrefetchValueOp1             -> \args -> opAllDone $ \[] -> do
    doPrefetchValueOp 1 args

  PrefetchByteArrayOp0         -> \args -> opAllDone $ \[] -> do
    doPrefetchByteArrayOp 0  args
  PrefetchMutableByteArrayOp0  -> \args -> opAllDone $ \[] -> do
    doPrefetchMutableByteArrayOp 0  args
  PrefetchAddrOp0              -> \args -> opAllDone $ \[] -> do
    doPrefetchAddrOp 0  args
  PrefetchValueOp0             -> \args -> opAllDone $ \[] -> do
    doPrefetchValueOp 0 args

-- Atomic read-modify-write
  FetchAddByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_Add mba ix (bWord platform) n
  FetchSubByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_Sub mba ix (bWord platform) n
  FetchAndByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_And mba ix (bWord platform) n
  FetchNandByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_Nand mba ix (bWord platform) n
  FetchOrByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_Or mba ix (bWord platform) n
  FetchXorByteArrayOp_Int -> \[mba, ix, n] -> opAllDone $ \[res] -> do
    doAtomicRMW res AMO_Xor mba ix (bWord platform) n
  AtomicReadByteArrayOp_Int -> \[mba, ix] -> opAllDone $ \[res] -> do
    doAtomicReadByteArray res mba ix (bWord platform)
  AtomicWriteByteArrayOp_Int -> \[mba, ix, val] -> opAllDone $ \[] -> do
    doAtomicWriteByteArray mba ix (bWord platform) val
  CasByteArrayOp_Int -> \[mba, ix, old, new] -> opAllDone $ \[res] -> do
    doCasByteArray res mba ix (bWord platform) old new

-- The rest just translate straightforwardly

  Int2WordOp      -> \args -> opNop args
  Word2IntOp      -> \args -> opNop args
  Int2AddrOp      -> \args -> opNop args
  Addr2IntOp      -> \args -> opNop args
  ChrOp           -> \args -> opNop args  -- Int# and Char# are rep'd the same
  OrdOp           -> \args -> opNop args

  Narrow8IntOp   -> \args -> opNarrow platform args (MO_SS_Conv, W8)
  Narrow16IntOp  -> \args -> opNarrow platform args (MO_SS_Conv, W16)
  Narrow32IntOp  -> \args -> opNarrow platform args (MO_SS_Conv, W32)
  Narrow8WordOp  -> \args -> opNarrow platform args (MO_UU_Conv, W8)
  Narrow16WordOp -> \args -> opNarrow platform args (MO_UU_Conv, W16)
  Narrow32WordOp -> \args -> opNarrow platform args (MO_UU_Conv, W32)

  DoublePowerOp  -> \args -> opCallish args MO_F64_Pwr
  DoubleSinOp    -> \args -> opCallish args MO_F64_Sin
  DoubleCosOp    -> \args -> opCallish args MO_F64_Cos
  DoubleTanOp    -> \args -> opCallish args MO_F64_Tan
  DoubleSinhOp   -> \args -> opCallish args MO_F64_Sinh
  DoubleCoshOp   -> \args -> opCallish args MO_F64_Cosh
  DoubleTanhOp   -> \args -> opCallish args MO_F64_Tanh
  DoubleAsinOp   -> \args -> opCallish args MO_F64_Asin
  DoubleAcosOp   -> \args -> opCallish args MO_F64_Acos
  DoubleAtanOp   -> \args -> opCallish args MO_F64_Atan
  DoubleAsinhOp  -> \args -> opCallish args MO_F64_Asinh
  DoubleAcoshOp  -> \args -> opCallish args MO_F64_Acosh
  DoubleAtanhOp  -> \args -> opCallish args MO_F64_Atanh
  DoubleLogOp    -> \args -> opCallish args MO_F64_Log
  DoubleLog1POp  -> \args -> opCallish args MO_F64_Log1P
  DoubleExpOp    -> \args -> opCallish args MO_F64_Exp
  DoubleExpM1Op  -> \args -> opCallish args MO_F64_ExpM1
  DoubleSqrtOp   -> \args -> opCallish args MO_F64_Sqrt

  FloatPowerOp   -> \args -> opCallish args MO_F32_Pwr
  FloatSinOp     -> \args -> opCallish args MO_F32_Sin
  FloatCosOp     -> \args -> opCallish args MO_F32_Cos
  FloatTanOp     -> \args -> opCallish args MO_F32_Tan
  FloatSinhOp    -> \args -> opCallish args MO_F32_Sinh
  FloatCoshOp    -> \args -> opCallish args MO_F32_Cosh
  FloatTanhOp    -> \args -> opCallish args MO_F32_Tanh
  FloatAsinOp    -> \args -> opCallish args MO_F32_Asin
  FloatAcosOp    -> \args -> opCallish args MO_F32_Acos
  FloatAtanOp    -> \args -> opCallish args MO_F32_Atan
  FloatAsinhOp   -> \args -> opCallish args MO_F32_Asinh
  FloatAcoshOp   -> \args -> opCallish args MO_F32_Acosh
  FloatAtanhOp   -> \args -> opCallish args MO_F32_Atanh
  FloatLogOp     -> \args -> opCallish args MO_F32_Log
  FloatLog1POp   -> \args -> opCallish args MO_F32_Log1P
  FloatExpOp     -> \args -> opCallish args MO_F32_Exp
  FloatExpM1Op   -> \args -> opCallish args MO_F32_ExpM1
  FloatSqrtOp    -> \args -> opCallish args MO_F32_Sqrt

-- Native word signless ops

  IntAddOp       -> \args -> opTranslate args (mo_wordAdd platform)
  IntSubOp       -> \args -> opTranslate args (mo_wordSub platform)
  WordAddOp      -> \args -> opTranslate args (mo_wordAdd platform)
  WordSubOp      -> \args -> opTranslate args (mo_wordSub platform)
  AddrAddOp      -> \args -> opTranslate args (mo_wordAdd platform)
  AddrSubOp      -> \args -> opTranslate args (mo_wordSub platform)

  IntEqOp        -> \args -> opTranslate args (mo_wordEq platform)
  IntNeOp        -> \args -> opTranslate args (mo_wordNe platform)
  WordEqOp       -> \args -> opTranslate args (mo_wordEq platform)
  WordNeOp       -> \args -> opTranslate args (mo_wordNe platform)
  AddrEqOp       -> \args -> opTranslate args (mo_wordEq platform)
  AddrNeOp       -> \args -> opTranslate args (mo_wordNe platform)

  AndOp          -> \args -> opTranslate args (mo_wordAnd platform)
  OrOp           -> \args -> opTranslate args (mo_wordOr platform)
  XorOp          -> \args -> opTranslate args (mo_wordXor platform)
  NotOp          -> \args -> opTranslate args (mo_wordNot platform)
  SllOp          -> \args -> opTranslate args (mo_wordShl platform)
  SrlOp          -> \args -> opTranslate args (mo_wordUShr platform)

  AddrRemOp      -> \args -> opTranslate args (mo_wordURem platform)

-- Native word signed ops

  IntMulOp        -> \args -> opTranslate args (mo_wordMul platform)
  IntMulMayOfloOp -> \args -> opTranslate args (MO_S_MulMayOflo (wordWidth platform))
  IntQuotOp       -> \args -> opTranslate args (mo_wordSQuot platform)
  IntRemOp        -> \args -> opTranslate args (mo_wordSRem platform)
  IntNegOp        -> \args -> opTranslate args (mo_wordSNeg platform)

  IntGeOp        -> \args -> opTranslate args (mo_wordSGe platform)
  IntLeOp        -> \args -> opTranslate args (mo_wordSLe platform)
  IntGtOp        -> \args -> opTranslate args (mo_wordSGt platform)
  IntLtOp        -> \args -> opTranslate args (mo_wordSLt platform)

  AndIOp         -> \args -> opTranslate args (mo_wordAnd platform)
  OrIOp          -> \args -> opTranslate args (mo_wordOr platform)
  XorIOp         -> \args -> opTranslate args (mo_wordXor platform)
  NotIOp         -> \args -> opTranslate args (mo_wordNot platform)
  ISllOp         -> \args -> opTranslate args (mo_wordShl platform)
  ISraOp         -> \args -> opTranslate args (mo_wordSShr platform)
  ISrlOp         -> \args -> opTranslate args (mo_wordUShr platform)

-- Native word unsigned ops

  WordGeOp       -> \args -> opTranslate args (mo_wordUGe platform)
  WordLeOp       -> \args -> opTranslate args (mo_wordULe platform)
  WordGtOp       -> \args -> opTranslate args (mo_wordUGt platform)
  WordLtOp       -> \args -> opTranslate args (mo_wordULt platform)

  WordMulOp      -> \args -> opTranslate args (mo_wordMul platform)
  WordQuotOp     -> \args -> opTranslate args (mo_wordUQuot platform)
  WordRemOp      -> \args -> opTranslate args (mo_wordURem platform)

  AddrGeOp       -> \args -> opTranslate args (mo_wordUGe platform)
  AddrLeOp       -> \args -> opTranslate args (mo_wordULe platform)
  AddrGtOp       -> \args -> opTranslate args (mo_wordUGt platform)
  AddrLtOp       -> \args -> opTranslate args (mo_wordULt platform)

-- Int8# signed ops

  Int8Extend     -> \args -> opTranslate args (MO_SS_Conv W8 (wordWidth platform))
  Int8Narrow     -> \args -> opTranslate args (MO_SS_Conv (wordWidth platform) W8)
  Int8NegOp      -> \args -> opTranslate args (MO_S_Neg W8)
  Int8AddOp      -> \args -> opTranslate args (MO_Add W8)
  Int8SubOp      -> \args -> opTranslate args (MO_Sub W8)
  Int8MulOp      -> \args -> opTranslate args (MO_Mul W8)
  Int8QuotOp     -> \args -> opTranslate args (MO_S_Quot W8)
  Int8RemOp      -> \args -> opTranslate args (MO_S_Rem W8)

  Int8EqOp       -> \args -> opTranslate args (MO_Eq W8)
  Int8GeOp       -> \args -> opTranslate args (MO_S_Ge W8)
  Int8GtOp       -> \args -> opTranslate args (MO_S_Gt W8)
  Int8LeOp       -> \args -> opTranslate args (MO_S_Le W8)
  Int8LtOp       -> \args -> opTranslate args (MO_S_Lt W8)
  Int8NeOp       -> \args -> opTranslate args (MO_Ne W8)

-- Word8# unsigned ops

  Word8Extend     -> \args -> opTranslate args (MO_UU_Conv W8 (wordWidth platform))
  Word8Narrow     -> \args -> opTranslate args (MO_UU_Conv (wordWidth platform) W8)
  Word8NotOp      -> \args -> opTranslate args (MO_Not W8)
  Word8AddOp      -> \args -> opTranslate args (MO_Add W8)
  Word8SubOp      -> \args -> opTranslate args (MO_Sub W8)
  Word8MulOp      -> \args -> opTranslate args (MO_Mul W8)
  Word8QuotOp     -> \args -> opTranslate args (MO_U_Quot W8)
  Word8RemOp      -> \args -> opTranslate args (MO_U_Rem W8)

  Word8EqOp       -> \args -> opTranslate args (MO_Eq W8)
  Word8GeOp       -> \args -> opTranslate args (MO_U_Ge W8)
  Word8GtOp       -> \args -> opTranslate args (MO_U_Gt W8)
  Word8LeOp       -> \args -> opTranslate args (MO_U_Le W8)
  Word8LtOp       -> \args -> opTranslate args (MO_U_Lt W8)
  Word8NeOp       -> \args -> opTranslate args (MO_Ne W8)

-- Int16# signed ops

  Int16Extend     -> \args -> opTranslate args (MO_SS_Conv W16 (wordWidth platform))
  Int16Narrow     -> \args -> opTranslate args (MO_SS_Conv (wordWidth platform) W16)
  Int16NegOp      -> \args -> opTranslate args (MO_S_Neg W16)
  Int16AddOp      -> \args -> opTranslate args (MO_Add W16)
  Int16SubOp      -> \args -> opTranslate args (MO_Sub W16)
  Int16MulOp      -> \args -> opTranslate args (MO_Mul W16)
  Int16QuotOp     -> \args -> opTranslate args (MO_S_Quot W16)
  Int16RemOp      -> \args -> opTranslate args (MO_S_Rem W16)

  Int16EqOp       -> \args -> opTranslate args (MO_Eq W16)
  Int16GeOp       -> \args -> opTranslate args (MO_S_Ge W16)
  Int16GtOp       -> \args -> opTranslate args (MO_S_Gt W16)
  Int16LeOp       -> \args -> opTranslate args (MO_S_Le W16)
  Int16LtOp       -> \args -> opTranslate args (MO_S_Lt W16)
  Int16NeOp       -> \args -> opTranslate args (MO_Ne W16)

-- Word16# unsigned ops

  Word16Extend     -> \args -> opTranslate args (MO_UU_Conv W16 (wordWidth platform))
  Word16Narrow     -> \args -> opTranslate args (MO_UU_Conv (wordWidth platform) W16)
  Word16NotOp      -> \args -> opTranslate args (MO_Not W16)
  Word16AddOp      -> \args -> opTranslate args (MO_Add W16)
  Word16SubOp      -> \args -> opTranslate args (MO_Sub W16)
  Word16MulOp      -> \args -> opTranslate args (MO_Mul W16)
  Word16QuotOp     -> \args -> opTranslate args (MO_U_Quot W16)
  Word16RemOp      -> \args -> opTranslate args (MO_U_Rem W16)

  Word16EqOp       -> \args -> opTranslate args (MO_Eq W16)
  Word16GeOp       -> \args -> opTranslate args (MO_U_Ge W16)
  Word16GtOp       -> \args -> opTranslate args (MO_U_Gt W16)
  Word16LeOp       -> \args -> opTranslate args (MO_U_Le W16)
  Word16LtOp       -> \args -> opTranslate args (MO_U_Lt W16)
  Word16NeOp       -> \args -> opTranslate args (MO_Ne W16)

-- Char# ops

  CharEqOp       -> \args -> opTranslate args (MO_Eq (wordWidth platform))
  CharNeOp       -> \args -> opTranslate args (MO_Ne (wordWidth platform))
  CharGeOp       -> \args -> opTranslate args (MO_U_Ge (wordWidth platform))
  CharLeOp       -> \args -> opTranslate args (MO_U_Le (wordWidth platform))
  CharGtOp       -> \args -> opTranslate args (MO_U_Gt (wordWidth platform))
  CharLtOp       -> \args -> opTranslate args (MO_U_Lt (wordWidth platform))

-- Double ops

  DoubleEqOp     -> \args -> opTranslate args (MO_F_Eq W64)
  DoubleNeOp     -> \args -> opTranslate args (MO_F_Ne W64)
  DoubleGeOp     -> \args -> opTranslate args (MO_F_Ge W64)
  DoubleLeOp     -> \args -> opTranslate args (MO_F_Le W64)
  DoubleGtOp     -> \args -> opTranslate args (MO_F_Gt W64)
  DoubleLtOp     -> \args -> opTranslate args (MO_F_Lt W64)

  DoubleAddOp    -> \args -> opTranslate args (MO_F_Add W64)
  DoubleSubOp    -> \args -> opTranslate args (MO_F_Sub W64)
  DoubleMulOp    -> \args -> opTranslate args (MO_F_Mul W64)
  DoubleDivOp    -> \args -> opTranslate args (MO_F_Quot W64)
  DoubleNegOp    -> \args -> opTranslate args (MO_F_Neg W64)

-- Float ops

  FloatEqOp     -> \args -> opTranslate args (MO_F_Eq W32)
  FloatNeOp     -> \args -> opTranslate args (MO_F_Ne W32)
  FloatGeOp     -> \args -> opTranslate args (MO_F_Ge W32)
  FloatLeOp     -> \args -> opTranslate args (MO_F_Le W32)
  FloatGtOp     -> \args -> opTranslate args (MO_F_Gt W32)
  FloatLtOp     -> \args -> opTranslate args (MO_F_Lt W32)

  FloatAddOp    -> \args -> opTranslate args (MO_F_Add  W32)
  FloatSubOp    -> \args -> opTranslate args (MO_F_Sub  W32)
  FloatMulOp    -> \args -> opTranslate args (MO_F_Mul  W32)
  FloatDivOp    -> \args -> opTranslate args (MO_F_Quot W32)
  FloatNegOp    -> \args -> opTranslate args (MO_F_Neg  W32)

-- Vector ops

  (VecAddOp  FloatVec n w) -> \args -> opTranslate args (MO_VF_Add  n w)
  (VecSubOp  FloatVec n w) -> \args -> opTranslate args (MO_VF_Sub  n w)
  (VecMulOp  FloatVec n w) -> \args -> opTranslate args (MO_VF_Mul  n w)
  (VecDivOp  FloatVec n w) -> \args -> opTranslate args (MO_VF_Quot n w)
  (VecQuotOp FloatVec _ _) -> \_ -> panic "unsupported primop"
  (VecRemOp  FloatVec _ _) -> \_ -> panic "unsupported primop"
  (VecNegOp  FloatVec n w) -> \args -> opTranslate args (MO_VF_Neg  n w)

  (VecAddOp  IntVec n w) -> \args -> opTranslate args (MO_V_Add   n w)
  (VecSubOp  IntVec n w) -> \args -> opTranslate args (MO_V_Sub   n w)
  (VecMulOp  IntVec n w) -> \args -> opTranslate args (MO_V_Mul   n w)
  (VecDivOp  IntVec _ _) -> \_ -> panic "unsupported primop"
  (VecQuotOp IntVec n w) -> \args -> opTranslate args (MO_VS_Quot n w)
  (VecRemOp  IntVec n w) -> \args -> opTranslate args (MO_VS_Rem  n w)
  (VecNegOp  IntVec n w) -> \args -> opTranslate args (MO_VS_Neg  n w)

  (VecAddOp  WordVec n w) -> \args -> opTranslate args (MO_V_Add   n w)
  (VecSubOp  WordVec n w) -> \args -> opTranslate args (MO_V_Sub   n w)
  (VecMulOp  WordVec n w) -> \args -> opTranslate args (MO_V_Mul   n w)
  (VecDivOp  WordVec _ _) -> \_ -> panic "unsupported primop"
  (VecQuotOp WordVec n w) -> \args -> opTranslate args (MO_VU_Quot n w)
  (VecRemOp  WordVec n w) -> \args -> opTranslate args (MO_VU_Rem  n w)
  (VecNegOp  WordVec _ _) -> \_ -> panic "unsupported primop"

-- Conversions

  Int2DoubleOp   -> \args -> opTranslate args (MO_SF_Conv (wordWidth platform) W64)
  Double2IntOp   -> \args -> opTranslate args (MO_FS_Conv W64 (wordWidth platform))

  Int2FloatOp    -> \args -> opTranslate args (MO_SF_Conv (wordWidth platform) W32)
  Float2IntOp    -> \args -> opTranslate args (MO_FS_Conv W32 (wordWidth platform))

  Float2DoubleOp -> \args -> opTranslate args (MO_FF_Conv W32 W64)
  Double2FloatOp -> \args -> opTranslate args (MO_FF_Conv W64 W32)

-- Word comparisons masquerading as more exotic things.

  SameMutVarOp            -> \args -> opTranslate args (mo_wordEq platform)
  SameMVarOp              -> \args -> opTranslate args (mo_wordEq platform)
  SameMutableArrayOp      -> \args -> opTranslate args (mo_wordEq platform)
  SameMutableByteArrayOp  -> \args -> opTranslate args (mo_wordEq platform)
  SameMutableArrayArrayOp -> \args -> opTranslate args (mo_wordEq platform)
  SameSmallMutableArrayOp -> \args -> opTranslate args (mo_wordEq platform)
  SameTVarOp              -> \args -> opTranslate args (mo_wordEq platform)
  EqStablePtrOp           -> \args -> opTranslate args (mo_wordEq platform)
-- See Note [Comparing stable names]
  EqStableNameOp          -> \args -> opTranslate args (mo_wordEq platform)

  IntQuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_S_QuotRem  (wordWidth platform))
    else Right (genericIntQuotRemOp (wordWidth platform))

  Int8QuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_S_QuotRem W8)
    else Right (genericIntQuotRemOp W8)

  Int16QuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_S_QuotRem W16)
    else Right (genericIntQuotRemOp W16)

  WordQuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_U_QuotRem  (wordWidth platform))
    else Right (genericWordQuotRemOp (wordWidth platform))

  WordQuotRem2Op -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_U_QuotRem2 (wordWidth platform))
    else Right (genericWordQuotRem2Op platform)

  Word8QuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_U_QuotRem W8)
    else Right (genericWordQuotRemOp W8)

  Word16QuotRemOp -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) && not (quotRemCanBeOptimized args)
    then Left (MO_U_QuotRem W16)
    else Right (genericWordQuotRemOp W16)

  WordAdd2Op -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_Add2       (wordWidth platform))
    else Right genericWordAdd2Op

  WordAddCOp -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_AddWordC   (wordWidth platform))
    else Right genericWordAddCOp

  WordSubCOp -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_SubWordC   (wordWidth platform))
    else Right genericWordSubCOp

  IntAddCOp -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_AddIntC    (wordWidth platform))
    else Right genericIntAddCOp

  IntSubCOp -> \args -> opCallishHandledLater args $
    if (ncg && (x86ish || ppc)) || llvm
    then Left (MO_SubIntC    (wordWidth platform))
    else Right genericIntSubCOp

  WordMul2Op -> \args -> opCallishHandledLater args $
    if ncg && (x86ish || ppc) || llvm
    then Left (MO_U_Mul2     (wordWidth platform))
    else Right genericWordMul2Op

  IntMul2Op  -> \args -> opCallishHandledLater args $
    if ncg && x86ish
    then Left (MO_S_Mul2     (wordWidth platform))
    else Right genericIntMul2Op

  FloatFabsOp -> \args -> opCallishHandledLater args $
    if (ncg && x86ish || ppc) || llvm
    then Left MO_F32_Fabs
    else Right $ genericFabsOp W32

  DoubleFabsOp -> \args -> opCallishHandledLater args $
    if (ncg && x86ish || ppc) || llvm
    then Left MO_F64_Fabs
    else Right $ genericFabsOp W64

  -- tagToEnum# is special: we need to pull the constructor
  -- out of the table, and perform an appropriate return.
  TagToEnumOp -> \[amode] -> PrimopCmmEmit_Raw $ \res_ty -> do
    -- If you're reading this code in the attempt to figure
    -- out why the compiler panic'ed here, it is probably because
    -- you used tagToEnum# in a non-monomorphic setting, e.g.,
    --         intToTg :: Enum a => Int -> a ; intToTg (I# x#) = tagToEnum# x#
    -- That won't work.
    let tycon = tyConAppTyCon res_ty
    MASSERT(isEnumerationTyCon tycon)
    platform <- getPlatform
    pure [tagToClosure platform tycon amode]

-- Out of line primops.
-- TODO compiler need not know about these

  UnsafeThawArrayOp -> alwaysExternal
  CasArrayOp -> alwaysExternal
  UnsafeThawSmallArrayOp -> alwaysExternal
  CasSmallArrayOp -> alwaysExternal
  NewPinnedByteArrayOp_Char -> alwaysExternal
  NewAlignedPinnedByteArrayOp_Char -> alwaysExternal
  MutableByteArrayIsPinnedOp -> alwaysExternal
  DoubleDecode_2IntOp -> alwaysExternal
  DoubleDecode_Int64Op -> alwaysExternal
  FloatDecode_IntOp -> alwaysExternal
  ByteArrayIsPinnedOp -> alwaysExternal
  ShrinkMutableByteArrayOp_Char -> alwaysExternal
  ResizeMutableByteArrayOp_Char -> alwaysExternal
  ShrinkSmallMutableArrayOp_Char -> alwaysExternal
  NewArrayArrayOp -> alwaysExternal
  NewMutVarOp -> alwaysExternal
  AtomicModifyMutVar2Op -> alwaysExternal
  AtomicModifyMutVar_Op -> alwaysExternal
  CasMutVarOp -> alwaysExternal
  CatchOp -> alwaysExternal
  RaiseOp -> alwaysExternal
  RaiseDivZeroOp -> alwaysExternal
  RaiseUnderflowOp -> alwaysExternal
  RaiseOverflowOp -> alwaysExternal
  RaiseIOOp -> alwaysExternal
  MaskAsyncExceptionsOp -> alwaysExternal
  MaskUninterruptibleOp -> alwaysExternal
  UnmaskAsyncExceptionsOp -> alwaysExternal
  MaskStatus -> alwaysExternal
  AtomicallyOp -> alwaysExternal
  RetryOp -> alwaysExternal
  CatchRetryOp -> alwaysExternal
  CatchSTMOp -> alwaysExternal
  NewTVarOp -> alwaysExternal
  ReadTVarOp -> alwaysExternal
  ReadTVarIOOp -> alwaysExternal
  WriteTVarOp -> alwaysExternal
  NewMVarOp -> alwaysExternal
  TakeMVarOp -> alwaysExternal
  TryTakeMVarOp -> alwaysExternal
  PutMVarOp -> alwaysExternal
  TryPutMVarOp -> alwaysExternal
  ReadMVarOp -> alwaysExternal
  TryReadMVarOp -> alwaysExternal
  IsEmptyMVarOp -> alwaysExternal
  DelayOp -> alwaysExternal
  WaitReadOp -> alwaysExternal
  WaitWriteOp -> alwaysExternal
  ForkOp -> alwaysExternal
  ForkOnOp -> alwaysExternal
  KillThreadOp -> alwaysExternal
  YieldOp -> alwaysExternal
  LabelThreadOp -> alwaysExternal
  IsCurrentThreadBoundOp -> alwaysExternal
  NoDuplicateOp -> alwaysExternal
  ThreadStatusOp -> alwaysExternal
  MkWeakOp -> alwaysExternal
  MkWeakNoFinalizerOp -> alwaysExternal
  AddCFinalizerToWeakOp -> alwaysExternal
  DeRefWeakOp -> alwaysExternal
  FinalizeWeakOp -> alwaysExternal
  MakeStablePtrOp -> alwaysExternal
  DeRefStablePtrOp -> alwaysExternal
  MakeStableNameOp -> alwaysExternal
  CompactNewOp -> alwaysExternal
  CompactResizeOp -> alwaysExternal
  CompactContainsOp -> alwaysExternal
  CompactContainsAnyOp -> alwaysExternal
  CompactGetFirstBlockOp -> alwaysExternal
  CompactGetNextBlockOp -> alwaysExternal
  CompactAllocateBlockOp -> alwaysExternal
  CompactFixupPointersOp -> alwaysExternal
  CompactAdd -> alwaysExternal
  CompactAddWithSharing -> alwaysExternal
  CompactSize -> alwaysExternal
  SeqOp -> alwaysExternal
  GetSparkOp -> alwaysExternal
  NumSparks -> alwaysExternal
  DataToTagOp -> alwaysExternal
  MkApUpd0_Op -> alwaysExternal
  NewBCOOp -> alwaysExternal
  UnpackClosureOp -> alwaysExternal
  ClosureSizeOp -> alwaysExternal
  GetApStackValOp -> alwaysExternal
  ClearCCSOp -> alwaysExternal
  TraceEventOp -> alwaysExternal
  TraceEventBinaryOp -> alwaysExternal
  TraceMarkerOp -> alwaysExternal
  SetThreadAllocationCounter -> alwaysExternal

 where
  platform = targetPlatform dflags
  alwaysExternal = \_ -> PrimopCmmEmit_External
  -- Note [QuotRem optimization]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  --
  -- `quot` and `rem` with constant divisor can be implemented with fast bit-ops
  -- (shift, .&.).
  --
  -- Currently we only support optimization (performed in GHC.Cmm.Opt) when the
  -- constant is a power of 2. #9041 tracks the implementation of the general
  -- optimization.
  --
  -- `quotRem` can be optimized in the same way. However as it returns two values,
  -- it is implemented as a "callish" primop which is harder to match and
  -- to transform later on. For simplicity, the current implementation detects cases
  -- that can be optimized (see `quotRemCanBeOptimized`) and converts STG quotRem
  -- primop into two CMM quot and rem primops.
  quotRemCanBeOptimized = \case
    [_, CmmLit (CmmInt n _) ] -> isJust (exactLog2 n)
    _                         -> False

  ncg = case hscTarget dflags of
           HscAsm -> True
           _      -> False
  llvm = case hscTarget dflags of
           HscLlvm -> True
           _       -> False
  x86ish = case platformArch platform of
             ArchX86    -> True
             ArchX86_64 -> True
             _          -> False
  ppc = case platformArch platform of
          ArchPPC      -> True
          ArchPPC_64 _ -> True
          _            -> False

data PrimopCmmEmit
  = PrimopCmmEmit_External
  | PrimopCmmEmit_IntoRegs ([LocalReg] -- where to put the results
                           -> FCode ())
  -- | Manual escape hatch, this is just for the '@TagToEnum@'
  -- primop for now. It would be nice to remove this special case but that is
  -- future work.
  | PrimopCmmEmit_Raw (Type -- the return type, some primops are specialized to it
                       -> FCode [CmmExpr]) -- just for TagToEnum for now

opNop :: [CmmExpr] -> PrimopCmmEmit
opNop args = PrimopCmmEmit_IntoRegs $ \[res] -> emitAssign (CmmLocal res) arg
  where [arg] = args

opNarrow
  :: Platform
  -> [CmmExpr]
  -> (Width -> Width -> MachOp, Width)
  -> PrimopCmmEmit
opNarrow platform args (mop, rep) = PrimopCmmEmit_IntoRegs $ \[res] -> emitAssign (CmmLocal res) $
  CmmMachOp (mop rep (wordWidth platform)) [CmmMachOp (mop (wordWidth platform) rep) [arg]]
  where [arg] = args

-- | These primops are implemented by CallishMachOps, because they sometimes
-- turn into foreign calls depending on the backend.
opCallish :: [CmmExpr] -> CallishMachOp -> PrimopCmmEmit
opCallish args prim = PrimopCmmEmit_IntoRegs $ \[res] -> emitPrimCall [res] prim args

opTranslate :: [CmmExpr] -> MachOp -> PrimopCmmEmit
opTranslate args mop = PrimopCmmEmit_IntoRegs $ \[res] -> do
  let stmt = mkAssign (CmmLocal res) (CmmMachOp mop args)
  emit stmt

-- | Basically a "manual" case, rather than one of the common repetitive forms
-- above. The results are a parameter to the returned function so we know the
-- choice of variant never depends on them.
opCallishHandledLater
  :: [CmmExpr]
  -> Either CallishMachOp GenericOp
  -> PrimopCmmEmit
opCallishHandledLater args callOrNot = PrimopCmmEmit_IntoRegs $ \res0 -> case callOrNot of
  Left op   -> emit $ mkUnsafeCall (PrimTarget op) res0 args
  Right gen -> gen res0 args

opAllDone
  :: ([LocalReg] -- where to put the results
      -> FCode ())
  -> PrimopCmmEmit
opAllDone f = PrimopCmmEmit_IntoRegs $ f

type GenericOp = [CmmFormal] -> [CmmActual] -> FCode ()

genericIntQuotRemOp :: Width -> GenericOp
genericIntQuotRemOp width [res_q, res_r] [arg_x, arg_y]
   = emit $ mkAssign (CmmLocal res_q)
              (CmmMachOp (MO_S_Quot width) [arg_x, arg_y]) <*>
            mkAssign (CmmLocal res_r)
              (CmmMachOp (MO_S_Rem  width) [arg_x, arg_y])
genericIntQuotRemOp _ _ _ = panic "genericIntQuotRemOp"

genericWordQuotRemOp :: Width -> GenericOp
genericWordQuotRemOp width [res_q, res_r] [arg_x, arg_y]
    = emit $ mkAssign (CmmLocal res_q)
               (CmmMachOp (MO_U_Quot width) [arg_x, arg_y]) <*>
             mkAssign (CmmLocal res_r)
               (CmmMachOp (MO_U_Rem  width) [arg_x, arg_y])
genericWordQuotRemOp _ _ _ = panic "genericWordQuotRemOp"

genericWordQuotRem2Op :: Platform -> GenericOp
genericWordQuotRem2Op platform [res_q, res_r] [arg_x_high, arg_x_low, arg_y]
    = emit =<< f (widthInBits (wordWidth platform)) zero arg_x_high arg_x_low
    where    ty = cmmExprType platform arg_x_high
             shl   x i = CmmMachOp (MO_Shl   (wordWidth platform)) [x, i]
             shr   x i = CmmMachOp (MO_U_Shr (wordWidth platform)) [x, i]
             or    x y = CmmMachOp (MO_Or    (wordWidth platform)) [x, y]
             ge    x y = CmmMachOp (MO_U_Ge  (wordWidth platform)) [x, y]
             ne    x y = CmmMachOp (MO_Ne    (wordWidth platform)) [x, y]
             minus x y = CmmMachOp (MO_Sub   (wordWidth platform)) [x, y]
             times x y = CmmMachOp (MO_Mul   (wordWidth platform)) [x, y]
             zero   = lit 0
             one    = lit 1
             negone = lit (fromIntegral (platformWordSizeInBits platform) - 1)
             lit i = CmmLit (CmmInt i (wordWidth platform))

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
  = do platform <- getPlatform
       r1 <- newTemp (cmmExprType platform arg_x)
       r2 <- newTemp (cmmExprType platform arg_x)
       let topHalf x = CmmMachOp (MO_U_Shr (wordWidth platform)) [x, hww]
           toTopHalf x = CmmMachOp (MO_Shl (wordWidth platform)) [x, hww]
           bottomHalf x = CmmMachOp (MO_And (wordWidth platform)) [x, hwm]
           add x y = CmmMachOp (MO_Add (wordWidth platform)) [x, y]
           or x y = CmmMachOp (MO_Or (wordWidth platform)) [x, y]
           hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth platform)))
                                (wordWidth platform))
           hwm = CmmLit (CmmInt (halfWordMask platform) (wordWidth platform))
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

-- | Implements branchless recovery of the carry flag @c@ by checking the
-- leftmost bits of both inputs @a@ and @b@ and result @r = a + b@:
--
-- @
--    c = a&b | (a|b)&~r
-- @
--
-- https://brodowsky.it-sky.net/2015/04/02/how-to-recover-the-carry-bit/
genericWordAddCOp :: GenericOp
genericWordAddCOp [res_r, res_c] [aa, bb]
 = do platform <- getPlatform
      emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordAdd platform) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr platform) [
            CmmMachOp (mo_wordOr platform) [
              CmmMachOp (mo_wordAnd platform) [aa,bb],
              CmmMachOp (mo_wordAnd platform) [
                CmmMachOp (mo_wordOr platform) [aa,bb],
                CmmMachOp (mo_wordNot platform) [CmmReg (CmmLocal res_r)]
              ]
            ],
            mkIntExpr platform (platformWordSizeInBits platform - 1)
          ]
        ]
genericWordAddCOp _ _ = panic "genericWordAddCOp"

-- | Implements branchless recovery of the carry flag @c@ by checking the
-- leftmost bits of both inputs @a@ and @b@ and result @r = a - b@:
--
-- @
--    c = ~a&b | (~a|b)&r
-- @
--
-- https://brodowsky.it-sky.net/2015/04/02/how-to-recover-the-carry-bit/
genericWordSubCOp :: GenericOp
genericWordSubCOp [res_r, res_c] [aa, bb]
 = do platform <- getPlatform
      emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordSub platform) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr platform) [
            CmmMachOp (mo_wordOr platform) [
              CmmMachOp (mo_wordAnd platform) [
                CmmMachOp (mo_wordNot platform) [aa],
                bb
              ],
              CmmMachOp (mo_wordAnd platform) [
                CmmMachOp (mo_wordOr platform) [
                  CmmMachOp (mo_wordNot platform) [aa],
                  bb
                ],
                CmmReg (CmmLocal res_r)
              ]
            ],
            mkIntExpr platform (platformWordSizeInBits platform - 1)
          ]
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
 = do platform <- getPlatform
      emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordAdd platform) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr platform) [
                CmmMachOp (mo_wordAnd platform) [
                    CmmMachOp (mo_wordNot platform) [CmmMachOp (mo_wordXor platform) [aa,bb]],
                    CmmMachOp (mo_wordXor platform) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr platform (platformWordSizeInBits platform - 1)
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
 = do platform <- getPlatform
      emit $ catAGraphs [
        mkAssign (CmmLocal res_r) (CmmMachOp (mo_wordSub platform) [aa,bb]),
        mkAssign (CmmLocal res_c) $
          CmmMachOp (mo_wordUShr platform) [
                CmmMachOp (mo_wordAnd platform) [
                    CmmMachOp (mo_wordXor platform) [aa,bb],
                    CmmMachOp (mo_wordXor platform) [aa, CmmReg (CmmLocal res_r)]
                ],
                mkIntExpr platform (platformWordSizeInBits platform - 1)
          ]
        ]
genericIntSubCOp _ _ = panic "genericIntSubCOp"

genericWordMul2Op :: GenericOp
genericWordMul2Op [res_h, res_l] [arg_x, arg_y]
 = do platform <- getPlatform
      let t = cmmExprType platform arg_x
      xlyl <- liftM CmmLocal $ newTemp t
      xlyh <- liftM CmmLocal $ newTemp t
      xhyl <- liftM CmmLocal $ newTemp t
      r    <- liftM CmmLocal $ newTemp t
      -- This generic implementation is very simple and slow. We might
      -- well be able to do better, but for now this at least works.
      let topHalf x = CmmMachOp (MO_U_Shr (wordWidth platform)) [x, hww]
          toTopHalf x = CmmMachOp (MO_Shl (wordWidth platform)) [x, hww]
          bottomHalf x = CmmMachOp (MO_And (wordWidth platform)) [x, hwm]
          add x y = CmmMachOp (MO_Add (wordWidth platform)) [x, y]
          sum = foldl1 add
          mul x y = CmmMachOp (MO_Mul (wordWidth platform)) [x, y]
          or x y = CmmMachOp (MO_Or (wordWidth platform)) [x, y]
          hww = CmmLit (CmmInt (fromIntegral (widthInBits (halfWordWidth platform)))
                               (wordWidth platform))
          hwm = CmmLit (CmmInt (halfWordMask platform) (wordWidth platform))
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

genericIntMul2Op :: GenericOp
genericIntMul2Op [res_c, res_h, res_l] [arg_x, arg_y]
 = do dflags <- getDynFlags
      platform <- getPlatform
      -- Implement algorithm from Hacker's Delight, 2nd edition, p.174
      let t = cmmExprType platform arg_x
      p   <- newTemp t
      -- 1) compute the multiplication as if numbers were unsigned
      let wordMul2 = case emitPrimOp dflags WordMul2Op [arg_x,arg_y] of
            PrimopCmmEmit_External -> panic "Unsupported out-of-line WordMul2Op"
            PrimopCmmEmit_IntoRegs f -> f
            PrimopCmmEmit_Raw _ -> panic "Unsupported inline WordMul2Op"
      wordMul2 [p,res_l]
      -- 2) correct the high bits of the unsigned result
      let carryFill x = CmmMachOp (MO_S_Shr ww) [x, wwm1]
          sub x y     = CmmMachOp (MO_Sub   ww) [x, y]
          and x y     = CmmMachOp (MO_And   ww) [x, y]
          neq x y     = CmmMachOp (MO_Ne    ww) [x, y]
          f   x y     = (carryFill x) `and` y
          wwm1        = CmmLit (CmmInt (fromIntegral (widthInBits ww - 1)) ww)
          rl x        = CmmReg (CmmLocal x)
          ww          = wordWidth platform
      emit $ catAGraphs
             [ mkAssign (CmmLocal res_h) (rl p `sub` f arg_x arg_y `sub` f arg_y arg_x)
             , mkAssign (CmmLocal res_c) (rl res_h `neq` carryFill (rl res_l))
             ]
genericIntMul2Op _ _ = panic "genericIntMul2Op"

-- This replicates what we had in libraries/base/GHC/Float.hs:
--
--    abs x    | x == 0    = 0 -- handles (-0.0)
--             | x >  0    = x
--             | otherwise = negateFloat x
genericFabsOp :: Width -> GenericOp
genericFabsOp w [res_r] [aa]
 = do platform <- getPlatform
      let zero   = CmmLit (CmmFloat 0 w)

          eq x y = CmmMachOp (MO_F_Eq w) [x, y]
          gt x y = CmmMachOp (MO_F_Gt w) [x, y]

          neg x  = CmmMachOp (MO_F_Neg w) [x]

          g1 = catAGraphs [mkAssign (CmmLocal res_r) zero]
          g2 = catAGraphs [mkAssign (CmmLocal res_r) aa]

      res_t <- CmmLocal <$> newTemp (cmmExprType platform aa)
      let g3 = catAGraphs [mkAssign res_t aa,
                           mkAssign (CmmLocal res_r) (neg (CmmReg res_t))]

      g4 <- mkCmmIfThenElse (gt aa zero) g2 g3

      emit =<< mkCmmIfThenElse (eq aa zero) g1 g4

genericFabsOp _ _ _ = panic "genericFabsOp"

-- Note [Comparing stable names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- A StableName# is actually a pointer to a stable name object (SNO)
-- containing an index into the stable name table (SNT). We
-- used to compare StableName#s by following the pointers to the
-- SNOs and checking whether they held the same SNT indices. However,
-- this is not necessary: there is a one-to-one correspondence
-- between SNOs and entries in the SNT, so simple pointer equality
-- does the trick.

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
   = panic "GHC.StgToCmm.Prim: doIndexOffAddrOp"

doIndexOffAddrOpAs :: Maybe MachOp
                   -> CmmType
                   -> CmmType
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doIndexOffAddrOpAs maybe_post_read_cast rep idx_rep [res] [addr,idx]
   = mkBasicIndexedRead 0 maybe_post_read_cast rep res addr idx_rep idx
doIndexOffAddrOpAs _ _ _ _ _
   = panic "GHC.StgToCmm.Prim: doIndexOffAddrOpAs"

doIndexByteArrayOp :: Maybe MachOp
                   -> CmmType
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doIndexByteArrayOp maybe_post_read_cast rep [res] [addr,idx]
   = do dflags <- getDynFlags
        mkBasicIndexedRead (arrWordsHdrSize dflags) maybe_post_read_cast rep res addr rep idx
doIndexByteArrayOp _ _ _ _
   = panic "GHC.StgToCmm.Prim: doIndexByteArrayOp"

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
   = panic "GHC.StgToCmm.Prim: doIndexByteArrayOpAs"

doReadPtrArrayOp :: LocalReg
                 -> CmmExpr
                 -> CmmExpr
                 -> FCode ()
doReadPtrArrayOp res addr idx
   = do dflags <- getDynFlags
        platform <- getPlatform
        mkBasicIndexedRead (arrPtrsHdrSize dflags) Nothing (gcWord platform) res addr (gcWord platform) idx

doWriteOffAddrOp :: Maybe MachOp
                 -> CmmType
                 -> [LocalReg]
                 -> [CmmExpr]
                 -> FCode ()
doWriteOffAddrOp maybe_pre_write_cast idx_ty [] [addr,idx,val]
   = mkBasicIndexedWrite 0 maybe_pre_write_cast addr idx_ty idx val
doWriteOffAddrOp _ _ _ _
   = panic "GHC.StgToCmm.Prim: doWriteOffAddrOp"

doWriteByteArrayOp :: Maybe MachOp
                   -> CmmType
                   -> [LocalReg]
                   -> [CmmExpr]
                   -> FCode ()
doWriteByteArrayOp maybe_pre_write_cast idx_ty [] [addr,idx,val]
   = do dflags <- getDynFlags
        mkBasicIndexedWrite (arrWordsHdrSize dflags) maybe_pre_write_cast addr idx_ty idx val
doWriteByteArrayOp _ _ _ _
   = panic "GHC.StgToCmm.Prim: doWriteByteArrayOp"

doWritePtrArrayOp :: CmmExpr
                  -> CmmExpr
                  -> CmmExpr
                  -> FCode ()
doWritePtrArrayOp addr idx val
  = do dflags <- getDynFlags
       platform <- getPlatform
       let ty = cmmExprType platform val
           hdr_size = arrPtrsHdrSize dflags
       -- Update remembered set for non-moving collector
       whenUpdRemSetEnabled
           $ emitUpdRemSetPush (cmmLoadIndexOffExpr platform hdr_size ty addr ty idx)
       -- This write barrier is to ensure that the heap writes to the object
       -- referred to by val have happened before we write val into the array.
       -- See #12469 for details.
       emitPrimCall [] MO_WriteBarrier []
       mkBasicIndexedWrite hdr_size Nothing addr ty idx val
       emit (setInfo addr (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))
       -- the write barrier.  We must write a byte into the mark table:
       -- bits8[a + header_size + StgMutArrPtrs_size(a) + x >> N]
       emit $ mkStore (
         cmmOffsetExpr platform
          (cmmOffsetExprW platform (cmmOffsetB platform addr hdr_size)
                         (loadArrPtrsSize dflags addr))
          (CmmMachOp (mo_wordUShr platform) [idx,
                                           mkIntExpr platform (mUT_ARR_PTRS_CARD_BITS dflags)])
         ) (CmmLit (CmmInt 1 W8))

loadArrPtrsSize :: DynFlags -> CmmExpr -> CmmExpr
loadArrPtrsSize dflags addr = CmmLoad (cmmOffsetB platform addr off) (bWord platform)
 where off = fixedHdrSize dflags + oFFSET_StgMutArrPtrs_ptrs dflags
       platform = targetPlatform dflags

mkBasicIndexedRead :: ByteOff      -- Initial offset in bytes
                   -> Maybe MachOp -- Optional result cast
                   -> CmmType      -- Type of element we are accessing
                   -> LocalReg     -- Destination
                   -> CmmExpr      -- Base address
                   -> CmmType      -- Type of element by which we are indexing
                   -> CmmExpr      -- Index
                   -> FCode ()
mkBasicIndexedRead off Nothing ty res base idx_ty idx
   = do platform <- getPlatform
        emitAssign (CmmLocal res) (cmmLoadIndexOffExpr platform off ty base idx_ty idx)
mkBasicIndexedRead off (Just cast) ty res base idx_ty idx
   = do platform <- getPlatform
        emitAssign (CmmLocal res) (CmmMachOp cast [
                                   cmmLoadIndexOffExpr platform off ty base idx_ty idx])

mkBasicIndexedWrite :: ByteOff      -- Initial offset in bytes
                    -> Maybe MachOp -- Optional value cast
                    -> CmmExpr      -- Base address
                    -> CmmType      -- Type of element by which we are indexing
                    -> CmmExpr      -- Index
                    -> CmmExpr      -- Value to write
                    -> FCode ()
mkBasicIndexedWrite off Nothing base idx_ty idx val
   = do platform <- getPlatform
        emitStore (cmmIndexOffExpr platform off (typeWidth idx_ty) base idx) val
mkBasicIndexedWrite off (Just cast) base idx_ty idx val
   = mkBasicIndexedWrite off Nothing base idx_ty idx (CmmMachOp cast [val])

-- ----------------------------------------------------------------------------
-- Misc utils

cmmIndexOffExpr :: Platform
                -> ByteOff  -- Initial offset in bytes
                -> Width    -- Width of element by which we are indexing
                -> CmmExpr  -- Base address
                -> CmmExpr  -- Index
                -> CmmExpr
cmmIndexOffExpr platform off width base idx
   = cmmIndexExpr platform width (cmmOffsetB platform base off) idx

cmmLoadIndexOffExpr :: Platform
                    -> ByteOff  -- Initial offset in bytes
                    -> CmmType  -- Type of element we are accessing
                    -> CmmExpr  -- Base address
                    -> CmmType  -- Type of element by which we are indexing
                    -> CmmExpr  -- Index
                    -> CmmExpr
cmmLoadIndexOffExpr platform off ty base idx_ty idx
   = CmmLoad (cmmIndexOffExpr platform off (typeWidth idx_ty) base idx) ty

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

vecElemInjectCast :: Platform -> PrimOpVecCat -> Width -> Maybe MachOp
vecElemInjectCast _        FloatVec _   =  Nothing
vecElemInjectCast platform   IntVec   W8  =  Just (mo_WordTo8  platform)
vecElemInjectCast platform   IntVec   W16 =  Just (mo_WordTo16 platform)
vecElemInjectCast platform   IntVec   W32 =  Just (mo_WordTo32 platform)
vecElemInjectCast _        IntVec   W64 =  Nothing
vecElemInjectCast platform   WordVec  W8  =  Just (mo_WordTo8  platform)
vecElemInjectCast platform   WordVec  W16 =  Just (mo_WordTo16 platform)
vecElemInjectCast platform   WordVec  W32 =  Just (mo_WordTo32 platform)
vecElemInjectCast _        WordVec  W64 =  Nothing
vecElemInjectCast _        _        _   =  Nothing

vecElemProjectCast :: Platform -> PrimOpVecCat -> Width -> Maybe MachOp
vecElemProjectCast _        FloatVec _   =  Nothing
vecElemProjectCast platform   IntVec   W8  =  Just (mo_s_8ToWord  platform)
vecElemProjectCast platform   IntVec   W16 =  Just (mo_s_16ToWord platform)
vecElemProjectCast platform   IntVec   W32 =  Just (mo_s_32ToWord platform)
vecElemProjectCast _        IntVec   W64 =  Nothing
vecElemProjectCast platform   WordVec  W8  =  Just (mo_u_8ToWord  platform)
vecElemProjectCast platform   WordVec  W16 =  Just (mo_u_16ToWord platform)
vecElemProjectCast platform   WordVec  W32 =  Just (mo_u_32ToWord platform)
vecElemProjectCast _        WordVec  W64 =  Nothing
vecElemProjectCast _        _        _   =  Nothing


-- NOTE [SIMD Design for the future]
-- Check to make sure that we can generate code for the specified vector type
-- given the current set of dynamic flags.
-- Currently these checks are specific to x86 and x86_64 architecture.
-- This should be fixed!
-- In particular,
-- 1) Add better support for other architectures! (this may require a redesign)
-- 2) Decouple design choices from LLVM's pseudo SIMD model!
--   The high level LLVM naive rep makes per CPU family SIMD generation is own
--   optimization problem, and hides important differences in eg ARM vs x86_64 simd
-- 3) Depending on the architecture, the SIMD registers may also support general
--    computations on Float/Double/Word/Int scalars, but currently on
--    for example x86_64, we always put Word/Int (or sized) in GPR
--    (general purpose) registers. Would relaxing that allow for
--    useful optimization opportunities?
--      Phrased differently, it is worth experimenting with supporting
--    different register mapping strategies than we currently have, especially if
--    someday we want SIMD to be a first class denizen in GHC along with scalar
--    values!
--      The current design with respect to register mapping of scalars could
--    very well be the best,but exploring the  design space and doing careful
--    measurements is the only only way to validate that.
--      In some next generation CPU ISAs, notably RISC V, the SIMD extension
--    includes  support for a sort of run time CPU dependent vectorization parameter,
--    where a loop may act upon a single scalar each iteration OR some 2,4,8 ...
--    element chunk! Time will tell if that direction sees wide adoption,
--    but it is from that context that unifying our handling of simd and scalars
--    may benefit. It is not likely to benefit current architectures, though
--    it may very well be a design perspective that helps guide improving the NCG.


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
    platform <- getPlatform
    -- vector indices are always 32-bits
    let idx' :: CmmExpr
        idx' = CmmMachOp (MO_SS_Conv (wordWidth platform) W32) [idx]
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
   = panic "GHC.StgToCmm.Prim: doPrefetchByteArrayOp"

-- | Translate mutable byte array prefetch operations into proper primcalls.
doPrefetchMutableByteArrayOp :: Int
                      -> [CmmExpr]
                      -> FCode ()
doPrefetchMutableByteArrayOp locality  [addr,idx]
   = do dflags <- getDynFlags
        mkBasicPrefetch locality (arrWordsHdrSize dflags)  addr idx
doPrefetchMutableByteArrayOp _ _
   = panic "GHC.StgToCmm.Prim: doPrefetchByteArrayOp"

-- | Translate address prefetch operations into proper primcalls.
doPrefetchAddrOp ::Int
                 -> [CmmExpr]
                 -> FCode ()
doPrefetchAddrOp locality   [addr,idx]
   = mkBasicPrefetch locality 0  addr idx
doPrefetchAddrOp _ _
   = panic "GHC.StgToCmm.Prim: doPrefetchAddrOp"

-- | Translate value prefetch operations into proper primcalls.
doPrefetchValueOp :: Int
                 -> [CmmExpr]
                 -> FCode ()
doPrefetchValueOp  locality   [addr]
  =  do platform <- getPlatform
        mkBasicPrefetch locality 0 addr  (CmmLit (CmmInt 0 (wordWidth platform)))
doPrefetchValueOp _ _
  = panic "GHC.StgToCmm.Prim: doPrefetchValueOp"

-- | helper to generate prefetch primcalls
mkBasicPrefetch :: Int          -- Locality level 0-3
                -> ByteOff      -- Initial offset in bytes
                -> CmmExpr      -- Base address
                -> CmmExpr      -- Index
                -> FCode ()
mkBasicPrefetch locality off base idx
   = do platform <- getPlatform
        emitPrimCall [] (MO_Prefetch_Data locality) [cmmIndexExpr platform W8 (cmmOffsetB platform base off) idx]
        return ()

-- ----------------------------------------------------------------------------
-- Allocating byte arrays

-- | Takes a register to return the newly allocated array in and the
-- size of the new array in bytes. Allocates a new
-- 'MutableByteArray#'.
doNewByteArrayOp :: CmmFormal -> ByteOff -> FCode ()
doNewByteArrayOp res_r n = do
    dflags <- getDynFlags
    platform <- getPlatform

    let info_ptr = mkLblExpr mkArrWords_infoLabel
        rep = arrWordsRep platform n

    tickyAllocPrim (mkIntExpr platform (arrWordsHdrSize dflags))
        (mkIntExpr platform (nonHdrSize platform rep))
        (zeroExpr platform)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr cccsExpr
                     [ (mkIntExpr platform n,
                        hdr_size + oFFSET_StgArrBytes_bytes dflags)
                     ]

    emit $ mkAssign (CmmLocal res_r) base

-- ----------------------------------------------------------------------------
-- Comparing byte arrays

doCompareByteArraysOp :: LocalReg -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                     -> FCode ()
doCompareByteArraysOp res ba1 ba1_off ba2 ba2_off n = do
    dflags <- getDynFlags
    platform <- getPlatform
    ba1_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform ba1 (arrWordsHdrSize dflags)) ba1_off
    ba2_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform ba2 (arrWordsHdrSize dflags)) ba2_off

    -- short-cut in case of equal pointers avoiding a costly
    -- subroutine call to the memcmp(3) routine; the Cmm logic below
    -- results in assembly code being generated for
    --
    --   cmpPrefix10 :: ByteArray# -> ByteArray# -> Int#
    --   cmpPrefix10 ba1 ba2 = compareByteArrays# ba1 0# ba2 0# 10#
    --
    -- that looks like
    --
    --          leaq 16(%r14),%rax
    --          leaq 16(%rsi),%rbx
    --          xorl %ecx,%ecx
    --          cmpq %rbx,%rax
    --          je l_ptr_eq
    --
    --          ; NB: the common case (unequal pointers) falls-through
    --          ; the conditional jump, and therefore matches the
    --          ; usual static branch prediction convention of modern cpus
    --
    --          subq $8,%rsp
    --          movq %rbx,%rsi
    --          movq %rax,%rdi
    --          movl $10,%edx
    --          xorl %eax,%eax
    --          call memcmp
    --          addq $8,%rsp
    --          movslq %eax,%rax
    --          movq %rax,%rcx
    --  l_ptr_eq:
    --          movq %rcx,%rbx
    --          jmp *(%rbp)

    l_ptr_eq <- newBlockId
    l_ptr_ne <- newBlockId

    emit (mkAssign (CmmLocal res) (zeroExpr platform))
    emit (mkCbranch (cmmEqWord platform ba1_p ba2_p)
                    l_ptr_eq l_ptr_ne (Just False))

    emitLabel l_ptr_ne
    emitMemcmpCall res ba1_p ba2_p n 1

    emitLabel l_ptr_eq

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
    copy _src _dst dst_p src_p bytes align =
        emitMemcpyCall dst_p src_p bytes align

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
    copy src dst dst_p src_p bytes align = do
        platform <- getPlatform
        (moveCall, cpyCall) <- forkAltPair
            (getCode $ emitMemmoveCall dst_p src_p bytes align)
            (getCode $ emitMemcpyCall  dst_p src_p bytes align)
        emit =<< mkCmmIfThenElse (cmmEqWord platform src dst) moveCall cpyCall

emitCopyByteArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                      -> Alignment -> FCode ())
                  -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                  -> FCode ()
emitCopyByteArray copy src src_off dst dst_off n = do
    dflags <- getDynFlags
    platform <- getPlatform
    let byteArrayAlignment = wordAlignment platform
        srcOffAlignment = cmmExprAlignment src_off
        dstOffAlignment = cmmExprAlignment dst_off
        align = minimum [byteArrayAlignment, srcOffAlignment, dstOffAlignment]
    dst_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform dst (arrWordsHdrSize dflags)) dst_off
    src_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform src (arrWordsHdrSize dflags)) src_off
    copy src dst dst_p src_p n align

-- | Takes a source 'ByteArray#', an offset in the source array, a
-- destination 'Addr#', and the number of bytes to copy.  Copies the given
-- number of bytes from the source array to the destination memory region.
doCopyByteArrayToAddrOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
doCopyByteArrayToAddrOp src src_off dst_p bytes = do
    -- Use memcpy (we are allowed to assume the arrays aren't overlapping)
    dflags <- getDynFlags
    platform <- getPlatform
    src_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform src (arrWordsHdrSize dflags)) src_off
    emitMemcpyCall dst_p src_p bytes (mkAlignment 1)

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
    platform <- getPlatform
    dst_p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform dst (arrWordsHdrSize dflags)) dst_off
    emitMemcpyCall dst_p src_p bytes (mkAlignment 1)


-- ----------------------------------------------------------------------------
-- Setting byte arrays

-- | Takes a 'MutableByteArray#', an offset into the array, a length,
-- and a byte, and sets each of the selected bytes in the array to the
-- character.
doSetByteArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr
                 -> FCode ()
doSetByteArrayOp ba off len c = do
    dflags <- getDynFlags
    platform <- getPlatform

    let byteArrayAlignment = wordAlignment platform -- known since BA is allocated on heap
        offsetAlignment = cmmExprAlignment off
        align = min byteArrayAlignment offsetAlignment

    p <- assignTempE $ cmmOffsetExpr platform (cmmOffsetB platform ba (arrWordsHdrSize dflags)) off
    emitMemsetCall p c len align

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
    platform <- getPlatform

    let info_ptr = mkLblExpr info

    tickyAllocPrim (mkIntExpr platform (hdrSize dflags rep))
        (mkIntExpr platform (nonHdrSize platform rep))
        (zeroExpr platform)

    base <- allocHeapClosure rep info_ptr cccsExpr payload

    arr <- CmmLocal `fmap` newTemp (bWord platform)
    emit $ mkAssign arr base

    -- Initialise all elements of the array
    let mkOff off = cmmOffsetW platform (CmmReg arr) (hdrSizeW dflags rep + off)
        initialization = [ mkStore (mkOff off) init | off <- [0.. n - 1] ]
    emit (catAGraphs initialization)

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
        do platform <- getPlatform
           emitMemcpyCall dst_p src_p (mkIntExpr platform bytes)
               (wordAlignment platform)


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
        platform <- getPlatform
        (moveCall, cpyCall) <- forkAltPair
            (getCode $ emitMemmoveCall dst_p src_p (mkIntExpr platform bytes)
             (wordAlignment platform))
            (getCode $ emitMemcpyCall  dst_p src_p (mkIntExpr platform bytes)
             (wordAlignment platform))
        emit =<< mkCmmIfThenElse (cmmEqWord platform src dst) moveCall cpyCall

emitCopyArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> ByteOff
                  -> FCode ())  -- ^ copy function
              -> CmmExpr        -- ^ source array
              -> CmmExpr        -- ^ offset in source array
              -> CmmExpr        -- ^ destination array
              -> CmmExpr        -- ^ offset in destination array
              -> WordOff        -- ^ number of elements to copy
              -> FCode ()
emitCopyArray copy src0 src_off dst0 dst_off0 n =
    when (n /= 0) $ do
        dflags <- getDynFlags
        platform <- getPlatform

        -- Passed as arguments (be careful)
        src     <- assignTempE src0
        dst     <- assignTempE dst0
        dst_off <- assignTempE dst_off0

        -- Nonmoving collector write barrier
        emitCopyUpdRemSetPush platform (arrPtrsHdrSizeW dflags) dst dst_off n

        -- Set the dirty bit in the header.
        emit (setInfo dst (CmmLit (CmmLabel mkMAP_DIRTY_infoLabel)))

        dst_elems_p <- assignTempE $ cmmOffsetB platform dst
                       (arrPtrsHdrSize dflags)
        dst_p <- assignTempE $ cmmOffsetExprW platform dst_elems_p dst_off
        src_p <- assignTempE $ cmmOffsetExprW platform
                 (cmmOffsetB platform src (arrPtrsHdrSize dflags)) src_off
        let bytes = wordsToBytes platform n

        copy src dst dst_p src_p bytes

        -- The base address of the destination card table
        dst_cards_p <- assignTempE $ cmmOffsetExprW platform dst_elems_p
                       (loadArrPtrsSize dflags dst)

        emitSetCards dst_off dst_cards_p n

doCopySmallArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
                   -> FCode ()
doCopySmallArrayOp = emitCopySmallArray copy
  where
    -- Copy data (we assume the arrays aren't overlapping since
    -- they're of different types)
    copy _src _dst dst_p src_p bytes =
        do platform <- getPlatform
           emitMemcpyCall dst_p src_p (mkIntExpr platform bytes)
               (wordAlignment platform)


doCopySmallMutableArrayOp :: CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> WordOff
                          -> FCode ()
doCopySmallMutableArrayOp = emitCopySmallArray copy
  where
    -- The only time the memory might overlap is when the two arrays
    -- we were provided are the same array!
    -- TODO: Optimize branch for common case of no aliasing.
    copy src dst dst_p src_p bytes = do
        platform <- getPlatform
        (moveCall, cpyCall) <- forkAltPair
            (getCode $ emitMemmoveCall dst_p src_p (mkIntExpr platform bytes)
             (wordAlignment platform))
            (getCode $ emitMemcpyCall  dst_p src_p (mkIntExpr platform bytes)
             (wordAlignment platform))
        emit =<< mkCmmIfThenElse (cmmEqWord platform src dst) moveCall cpyCall

emitCopySmallArray :: (CmmExpr -> CmmExpr -> CmmExpr -> CmmExpr -> ByteOff
                       -> FCode ())  -- ^ copy function
                   -> CmmExpr        -- ^ source array
                   -> CmmExpr        -- ^ offset in source array
                   -> CmmExpr        -- ^ destination array
                   -> CmmExpr        -- ^ offset in destination array
                   -> WordOff        -- ^ number of elements to copy
                   -> FCode ()
emitCopySmallArray copy src0 src_off dst0 dst_off n =
    when (n /= 0) $ do
        dflags <- getDynFlags
        platform <- getPlatform

        -- Passed as arguments (be careful)
        src     <- assignTempE src0
        dst     <- assignTempE dst0

        -- Nonmoving collector write barrier
        emitCopyUpdRemSetPush platform (smallArrPtrsHdrSizeW dflags) dst dst_off n

        -- Set the dirty bit in the header.
        emit (setInfo dst (CmmLit (CmmLabel mkSMAP_DIRTY_infoLabel)))

        dst_p <- assignTempE $ cmmOffsetExprW platform
                 (cmmOffsetB platform dst (smallArrPtrsHdrSize dflags)) dst_off
        src_p <- assignTempE $ cmmOffsetExprW platform
                 (cmmOffsetB platform src (smallArrPtrsHdrSize dflags)) src_off
        let bytes = wordsToBytes platform n

        copy src dst dst_p src_p bytes

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy. Allocates a new array and
-- initializes it from the source array.
emitCloneArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> WordOff
               -> FCode ()
emitCloneArray info_p res_r src src_off n = do
    dflags <- getDynFlags
    platform <- getPlatform

    let info_ptr = mkLblExpr info_p
        rep = arrPtrsRep dflags n

    tickyAllocPrim (mkIntExpr platform (arrPtrsHdrSize dflags))
        (mkIntExpr platform (nonHdrSize platform rep))
        (zeroExpr platform)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr cccsExpr
                     [ (mkIntExpr platform n,
                        hdr_size + oFFSET_StgMutArrPtrs_ptrs dflags)
                     , (mkIntExpr platform (nonHdrSizeW rep),
                        hdr_size + oFFSET_StgMutArrPtrs_size dflags)
                     ]

    arr <- CmmLocal `fmap` newTemp (bWord platform)
    emit $ mkAssign arr base

    dst_p <- assignTempE $ cmmOffsetB platform (CmmReg arr)
             (arrPtrsHdrSize dflags)
    src_p <- assignTempE $ cmmOffsetExprW platform src
             (cmmAddWord platform
              (mkIntExpr platform (arrPtrsHdrSizeW dflags)) src_off)

    emitMemcpyCall dst_p src_p (mkIntExpr platform (wordsToBytes platform n))
        (wordAlignment platform)

    emit $ mkAssign (CmmLocal res_r) (CmmReg arr)

-- | Takes an info table label, a register to return the newly
-- allocated array in, a source array, an offset in the source array,
-- and the number of elements to copy. Allocates a new array and
-- initializes it from the source array.
emitCloneSmallArray :: CLabel -> CmmFormal -> CmmExpr -> CmmExpr -> WordOff
                    -> FCode ()
emitCloneSmallArray info_p res_r src src_off n = do
    dflags <- getDynFlags
    platform <- getPlatform

    let info_ptr = mkLblExpr info_p
        rep = smallArrPtrsRep n

    tickyAllocPrim (mkIntExpr platform (smallArrPtrsHdrSize dflags))
        (mkIntExpr platform (nonHdrSize platform rep))
        (zeroExpr platform)

    let hdr_size = fixedHdrSize dflags

    base <- allocHeapClosure rep info_ptr cccsExpr
                     [ (mkIntExpr platform n,
                        hdr_size + oFFSET_StgSmallMutArrPtrs_ptrs dflags)
                     ]

    arr <- CmmLocal `fmap` newTemp (bWord platform)
    emit $ mkAssign arr base

    dst_p <- assignTempE $ cmmOffsetB platform (CmmReg arr)
             (smallArrPtrsHdrSize dflags)
    src_p <- assignTempE $ cmmOffsetExprW platform src
             (cmmAddWord platform
              (mkIntExpr platform (smallArrPtrsHdrSizeW dflags)) src_off)

    emitMemcpyCall dst_p src_p (mkIntExpr platform (wordsToBytes platform n))
        (wordAlignment platform)

    emit $ mkAssign (CmmLocal res_r) (CmmReg arr)

-- | Takes and offset in the destination array, the base address of
-- the card table, and the number of elements affected (*not* the
-- number of cards). The number of elements may not be zero.
-- Marks the relevant cards as dirty.
emitSetCards :: CmmExpr -> CmmExpr -> WordOff -> FCode ()
emitSetCards dst_start dst_cards_start n = do
    dflags <- getDynFlags
    platform <- getPlatform
    start_card <- assignTempE $ cardCmm dflags dst_start
    let end_card = cardCmm dflags
                   (cmmSubWord platform
                    (cmmAddWord platform dst_start (mkIntExpr platform n))
                    (mkIntExpr platform 1))
    emitMemsetCall (cmmAddWord platform dst_cards_start start_card)
        (mkIntExpr platform 1)
        (cmmAddWord platform (cmmSubWord platform end_card start_card) (mkIntExpr platform 1))
        (mkAlignment 1) -- no alignment (1 byte)

-- Convert an element index to a card index
cardCmm :: DynFlags -> CmmExpr -> CmmExpr
cardCmm dflags i =
    cmmUShrWord platform i (mkIntExpr platform (mUT_ARR_PTRS_CARD_BITS dflags))
    where platform = targetPlatform dflags

------------------------------------------------------------------------------
-- SmallArray PrimOp implementations

doReadSmallPtrArrayOp :: LocalReg
                      -> CmmExpr
                      -> CmmExpr
                      -> FCode ()
doReadSmallPtrArrayOp res addr idx = do
    dflags <- getDynFlags
    platform <- getPlatform
    mkBasicIndexedRead (smallArrPtrsHdrSize dflags) Nothing (gcWord platform) res addr
        (gcWord platform) idx

doWriteSmallPtrArrayOp :: CmmExpr
                       -> CmmExpr
                       -> CmmExpr
                       -> FCode ()
doWriteSmallPtrArrayOp addr idx val = do
    dflags <- getDynFlags
    platform <- getPlatform
    let ty = cmmExprType platform val

    -- Update remembered set for non-moving collector
    tmp <- newTemp ty
    mkBasicIndexedRead (smallArrPtrsHdrSize dflags) Nothing ty tmp addr ty idx
    whenUpdRemSetEnabled $ emitUpdRemSetPush (CmmReg (CmmLocal tmp))

    emitPrimCall [] MO_WriteBarrier [] -- #12469
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
    platform <- getPlatform
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr platform (arrWordsHdrSize dflags)
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
    platform <- getPlatform
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr platform (arrWordsHdrSize dflags)
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
    platform <- getPlatform
    let width = typeWidth idx_ty
        addr  = cmmIndexOffExpr platform (arrWordsHdrSize dflags)
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
    platform <- getPlatform
    let width = (typeWidth idx_ty)
        addr = cmmIndexOffExpr platform (arrWordsHdrSize dflags)
               width mba idx
    emitPrimCall
        [ res ]
        (MO_Cmpxchg width)
        [ addr, old, new ]

------------------------------------------------------------------------------
-- Helpers for emitting function calls

-- | Emit a call to @memcpy@.
emitMemcpyCall :: CmmExpr -> CmmExpr -> CmmExpr -> Alignment -> FCode ()
emitMemcpyCall dst src n align = do
    emitPrimCall
        [ {-no results-} ]
        (MO_Memcpy (alignmentBytes align))
        [ dst, src, n ]

-- | Emit a call to @memmove@.
emitMemmoveCall :: CmmExpr -> CmmExpr -> CmmExpr -> Alignment -> FCode ()
emitMemmoveCall dst src n align = do
    emitPrimCall
        [ {- no results -} ]
        (MO_Memmove (alignmentBytes align))
        [ dst, src, n ]

-- | Emit a call to @memset@.  The second argument must fit inside an
-- unsigned char.
emitMemsetCall :: CmmExpr -> CmmExpr -> CmmExpr -> Alignment -> FCode ()
emitMemsetCall dst c n align = do
    emitPrimCall
        [ {- no results -} ]
        (MO_Memset (alignmentBytes align))
        [ dst, c, n ]

emitMemcmpCall :: LocalReg -> CmmExpr -> CmmExpr -> CmmExpr -> Int -> FCode ()
emitMemcmpCall res ptr1 ptr2 n align = do
    -- 'MO_Memcmp' is assumed to return an 32bit 'CInt' because all
    -- code-gens currently call out to the @memcmp(3)@ C function.
    -- This was easier than moving the sign-extensions into
    -- all the code-gens.
    platform <- getPlatform
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
                         (mo_s_32ToWord platform)
                         [(CmmReg (CmmLocal cres))])

emitBSwapCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitBSwapCall res x width = do
    emitPrimCall
        [ res ]
        (MO_BSwap width)
        [ x ]

emitBRevCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitBRevCall res x width = do
    emitPrimCall
        [ res ]
        (MO_BRev width)
        [ x ]

emitPopCntCall :: LocalReg -> CmmExpr -> Width -> FCode ()
emitPopCntCall res x width = do
    emitPrimCall
        [ res ]
        (MO_PopCnt width)
        [ x ]

emitPdepCall :: LocalReg -> CmmExpr -> CmmExpr -> Width -> FCode ()
emitPdepCall res x y width = do
    emitPrimCall
        [ res ]
        (MO_Pdep width)
        [ x, y ]

emitPextCall :: LocalReg -> CmmExpr -> CmmExpr -> Width -> FCode ()
emitPextCall res x y width = do
    emitPrimCall
        [ res ]
        (MO_Pext width)
        [ x, y ]

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

---------------------------------------------------------------------------
-- Pushing to the update remembered set
---------------------------------------------------------------------------

-- | Push a range of pointer-array elements that are about to be copied over to
-- the update remembered set.
emitCopyUpdRemSetPush :: Platform
                      -> WordOff    -- ^ array header size
                      -> CmmExpr    -- ^ destination array
                      -> CmmExpr    -- ^ offset in destination array (in words)
                      -> Int        -- ^ number of elements to copy
                      -> FCode ()
emitCopyUpdRemSetPush _platform _hdr_size _dst _dst_off 0 = return ()
emitCopyUpdRemSetPush platform hdr_size dst dst_off n =
    whenUpdRemSetEnabled $ do
        updfr_off <- getUpdFrameOff
        graph <- mkCall lbl (NativeNodeCall,NativeReturn) [] args updfr_off []
        emit graph
  where
    lbl = mkLblExpr $ mkPrimCallLabel
          $ PrimCall (fsLit "stg_copyArray_barrier") rtsUnitId
    args =
      [ mkIntExpr platform hdr_size
      , dst
      , dst_off
      , mkIntExpr platform n
      ]
