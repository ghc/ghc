{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-|
Module      :  GHC.Exts.Heap
Copyright   :  (c) 2012 Joachim Breitner
License     :  BSD3
Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>

With this module, you can investigate the heap representation of Haskell
values, i.e. to investigate sharing and lazy evaluation.
-}

module GHC.Exts.Heap (
    -- * Closure types
      Closure
    , LiftedClosure
    , GenClosure(..)
    , ClosureType(..)
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , HasHeapRep(getClosureDataWith)
    , getClosureData

    -- * Info Table types
    , StgInfoTable(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    , itblSize
    , peekItbl
    , pokeItbl

    -- * Cost Centre (profiling) types
    , StgTSOProfInfo(..)
    , IndexTable(..)
    , CostCentre(..)
    , CostCentreStack(..)

     -- * Closure inspection
    , getBoxedClosureData
    , allClosures

    -- * Boxes
    , Box(..)
    , asBox
    , areBoxesEqual
    ) where

import Prelude
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Constants
import GHC.Exts.Heap.ProfInfo.Types
#if defined(PROFILING)
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled
import GHC.Exts.Heap.InfoTableProf
#else
-- This import makes PeekProfInfo_ProfilingEnabled available in make-based
-- builds. See #15197 for details (even though the related patch didn't
-- seem to fix the issue).
-- GHC.Exts.Heap.Closures uses the same trick to include
-- GHC.Exts.Heap.InfoTableProf into make-based builds.
import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingEnabled ()

import GHC.Exts.Heap.ProfInfo.PeekProfInfo_ProfilingDisabled
import GHC.Exts.Heap.InfoTable
#endif
import GHC.Exts.Heap.Utils
import qualified GHC.Exts.Heap.FFIClosures as FFIClosures

import Control.Monad
import Data.Bits
import GHC.Exts
import GHC.Int
import GHC.Word

import Foreign

#include "ghcconfig.h"

-- | Some closures (e.g.TSOs) don't have corresponding types to represent them in Haskell.
-- So when we have a pointer to such closure that we want to inspect, we `unsafeCoerce` it
-- into the following `LiftedClosure` lifted type (could be any lifted type) so that the
-- appropriate `instance HasHeapRep (a :: TYPE 'LiftedRep)` is used to decode the closure.
data LiftedClosure

class HasHeapRep (a :: TYPE rep) where

    -- | Decode a closure to it's heap representation ('GenClosure').
    -- Inside a GHC context 'b' is usually a 'GHC.Exts.Heap.Closures.Box'
    -- containing a thunk or an evaluated heap object. Outside it can be e.g.
    -- a 'Word' for "raw" usage of pointers.

    getClosureDataWith ::
        (forall c . c -> b)
        -- ^ Convert any closure to some pointer type.
        -> a
        -- ^ Closure to decode.
        -> IO (GenClosure b)
        -- ^ Heap representation of the closure.

instance HasHeapRep (a :: TYPE 'LiftedRep) where
    getClosureDataWith = getClosureWith

instance HasHeapRep (a :: TYPE 'UnliftedRep) where
    getClosureDataWith k x = getClosureWith (k . unsafeCoerce#) (unsafeCoerce# x)

instance Int# ~ a => HasHeapRep (a :: TYPE 'IntRep) where
    getClosureDataWith _ x = return $
        IntClosure { ptipe = PInt, intVal = I# x }

instance Word# ~ a => HasHeapRep (a :: TYPE 'WordRep) where
    getClosureDataWith _ x = return $
        WordClosure { ptipe = PWord, wordVal = W# x }

instance Int64# ~ a => HasHeapRep (a :: TYPE 'Int64Rep) where
    getClosureDataWith _ x = return $
        Int64Closure { ptipe = PInt64, int64Val = I64# (unsafeCoerce# x) }

instance Word64# ~ a => HasHeapRep (a :: TYPE 'Word64Rep) where
    getClosureDataWith _ x = return $
        Word64Closure { ptipe = PWord64, word64Val = W64# (unsafeCoerce# x) }

instance Addr# ~ a => HasHeapRep (a :: TYPE 'AddrRep) where
    getClosureDataWith _ x = return $
        AddrClosure { ptipe = PAddr, addrVal = I# (unsafeCoerce# x) }

instance Float# ~ a => HasHeapRep (a :: TYPE 'FloatRep) where
    getClosureDataWith _ x = return $
        FloatClosure { ptipe = PFloat, floatVal = F# x }

instance Double# ~ a => HasHeapRep (a :: TYPE 'DoubleRep) where
    getClosureDataWith _ x = return $
        DoubleClosure { ptipe = PDouble, doubleVal = D# x }

-- | Deconstruct any closure's heap representation.
getClosureRaw
    :: (forall c . c -> b)
    -- ^ Convert any closure to some pointer type.
    -> a
    -- ^ Closure to deconstruct.
    -> IO (Ptr StgInfoTable, [Word], [b])
    -- ^ Tuple of:
    -- * A 'Ptr' to the info table
    -- * Non-pointer data of the closure.
    -- * Pointer data of the closure. These are the closures pointed to by the
    --   input closure, boxed with the given function. The pointers are
    --   collected in @Heap.c@.
getClosureRaw asBoxish x = do
    case unpackClosure# x of
-- This is a hack to cover the bootstrap compiler using the old version of
-- 'unpackClosure'. The new 'unpackClosure' return values are not merely
-- a reordering, so using the old version would not work.
#if MIN_VERSION_ghc_prim(0,5,3)
        (# iptr, dat, pointers #) -> do
#else
        (# iptr, pointers, dat #) -> do
#endif
             let nelems = (I# (sizeofByteArray# dat)) `div` wORD_SIZE
                 end = fromIntegral nelems - 1
                 rawWds = [W# (indexWordArray# dat i) | I# i <- [0.. end] ]
                 ptrList = [case indexArray# pointers i of (# ptr #) -> asBoxish ptr
                            | I# i <- [0..(I# (sizeofArray# pointers)) - 1]
                            ]
             pure (Ptr iptr, rawWds, ptrList)

getClosureData :: forall rep (a :: TYPE rep) . HasHeapRep a => a -> IO Closure
getClosureData = getClosureDataWith asBox

-- | Get the heap representation of a closure _at this moment_, even if it is
-- unevaluated or an indirection or other exotic stuff. Beware when passing
-- something to this function, the same caveats as for
-- 'GHC.Exts.Heap.Closures.asBox' apply.
--
-- For most use cases 'getClosureData' is an easier to use alternative.
getClosureWith :: forall a b.
    (forall c . c -> b)
    -- ^ Convert any closure to some pointer type.
    -> a
    -- ^ Closure to decode.
    -> IO (GenClosure b)
    -- ^ Heap representation of the closure.
getClosureWith asBoxish x = do
    (iptr, wds, pts) <- getClosureRaw asBoxish (unsafeCoerce# x)
    itbl <- peekItbl iptr
    -- The remaining words after the header
    let rawWds = drop (closureTypeHeaderSize (tipe itbl)) wds
    -- For data args in a pointers then non-pointers closure
    -- This is incorrect in non pointers-first setups
    -- not sure if that happens
        npts = drop (closureTypeHeaderSize (tipe itbl) + length pts) wds
    case tipe itbl of
        t | t >= CONSTR && t <= CONSTR_NOCAF -> do
            (p, m, n) <- dataConNames iptr
            if m == "GHC.ByteCode.Instr" && n == "BreakInfo"
              then pure $ UnsupportedClosure itbl
              else pure $ ConstrClosure itbl pts npts p m n

        t | t >= THUNK && t <= THUNK_STATIC -> do
            pure $ ThunkClosure itbl pts npts

        THUNK_SELECTOR -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to THUNK_SELECTOR"
            pure $ SelectorClosure itbl (head pts)

        t | t >= FUN && t <= FUN_STATIC -> do
            pure $ FunClosure itbl pts npts

        AP -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to AP"
            -- We expect at least the arity, n_args, and fun fields
            unless (length rawWds >= 2) $
                fail $ "Expected at least 2 raw words to AP"
            let splitWord = rawWds !! 0
            pure $ APClosure itbl
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (head pts) (tail pts)

        PAP -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to PAP"
            -- We expect at least the arity, n_args, and fun fields
            unless (length rawWds >= 2) $
                fail "Expected at least 2 raw words to PAP"
            let splitWord = rawWds !! 0
            pure $ PAPClosure itbl
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (head pts) (tail pts)

        AP_STACK -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to AP_STACK"
            pure $ APStackClosure itbl (head pts) (tail pts)

        IND -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to IND"
            pure $ IndClosure itbl (head pts)

        IND_STATIC -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to IND_STATIC"
            pure $ IndClosure itbl (head pts)

        BLACKHOLE -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to BLACKHOLE"
            pure $ BlackholeClosure itbl (head pts)

        BCO -> do
            unless (length pts >= 3) $
                fail $ "Expected at least 3 ptr argument to BCO, found "
                        ++ show (length pts)
            unless (length rawWds >= 4) $
                fail $ "Expected at least 4 words to BCO, found "
                        ++ show (length rawWds)
            let splitWord = rawWds !! 3
            pure $ BCOClosure itbl (pts !! 0) (pts !! 1) (pts !! 2)
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (drop 4 rawWds)

        ARR_WORDS -> do
            unless (length rawWds >= 1) $
                fail $ "Expected at least 1 words to ARR_WORDS, found "
                        ++ show (length rawWds)
            pure $ ArrWordsClosure itbl (head rawWds) (tail rawWds)

        t | t >= MUT_ARR_PTRS_CLEAN && t <= MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length rawWds >= 2) $
                fail $ "Expected at least 2 words to MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length rawWds)
            pure $ MutArrClosure itbl (rawWds !! 0) (rawWds !! 1) pts

        t | t >= SMALL_MUT_ARR_PTRS_CLEAN && t <= SMALL_MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length rawWds >= 1) $
                fail $ "Expected at least 1 word to SMALL_MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length rawWds)
            pure $ SmallMutArrClosure itbl (rawWds !! 0) pts

        t | t == MUT_VAR_CLEAN || t == MUT_VAR_DIRTY -> do
            unless (length pts >= 1) $
                fail $ "Expected at least 1 words to MUT_VAR, found "
                        ++ show (length pts)
            pure $ MutVarClosure itbl (head pts)

        t | t == MVAR_CLEAN || t == MVAR_DIRTY -> do
            unless (length pts >= 3) $
                fail $ "Expected at least 3 ptrs to MVAR, found "
                        ++ show (length pts)
            pure $ MVarClosure itbl (pts !! 0) (pts !! 1) (pts !! 2)

        BLOCKING_QUEUE ->
            pure $ OtherClosure itbl pts wds
        --    pure $ BlockingQueueClosure itbl
        --        (pts !! 0) (pts !! 1) (pts !! 2) (pts !! 3)

        --  pure $ OtherClosure itbl pts wds
        --
        WEAK ->
            pure $ WeakClosure
                { info = itbl
                , cfinalizers = pts !! 0
                , key = pts !! 1
                , value = pts !! 2
                , finalizer = pts !! 3
                , link = pts !! 4
                }
        TSO | [ u_lnk, u_gbl_lnk, tso_stack, u_trec, u_blk_ex, u_bq] <- pts
                -> withArray wds (\ptr -> do
                    fields <- FFIClosures.peekTSOFields peekStgTSOProfInfo ptr
                    pure $ TSOClosure
                        { info = itbl
                        , unsafe_link = u_lnk
                        , unsafe_global_link = u_gbl_lnk
                        , tsoStack = tso_stack
                        , unsafe_trec = u_trec
                        , unsafe_blocked_exceptions = u_blk_ex
                        , unsafe_bq = u_bq
                        , what_next = FFIClosures.tso_what_next fields
                        , why_blocked = FFIClosures.tso_why_blocked fields
                        , flags = FFIClosures.tso_flags fields
                        , threadId = FFIClosures.tso_threadId fields
                        , saved_errno = FFIClosures.tso_saved_errno fields
                        , tso_dirty = FFIClosures.tso_dirty fields
                        , alloc_limit = FFIClosures.tso_alloc_limit fields
                        , tot_stack_size = FFIClosures.tso_tot_stack_size fields
                        , prof = FFIClosures.tso_prof fields
                        })
            | otherwise
                -> fail $ "Expected 6 ptr arguments to TSO, found "
                        ++ show (length pts)
        STACK   | [u_sp] <- pts
                    -> withArray wds (\ptr -> do
                            fields <- FFIClosures.peekStackFields ptr

                            pure $ StackClosure
                                { info = itbl
                                , stack_size = FFIClosures.stack_size fields
                                , stack_dirty = FFIClosures.stack_dirty fields
                                , unsafeStackPointer = u_sp
                                , unsafeStack  = FFIClosures.stack fields
#if __GLASGOW_HASKELL__ >= 811
                                , stack_marking = FFIClosures.stack_marking fields
#endif
                                })
                | otherwise
                    -> fail $ "Expected 1 ptr argument to STACK, found "
                        ++ show (length pts)

        _ ->
            pure $ UnsupportedClosure itbl

-- | Like 'getClosureDataWith', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a
