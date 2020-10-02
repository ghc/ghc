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
    , HasHeapRep(getClosureData)
    , getClosureDataFromHeapRep

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
    getClosureData
        :: a
        -- ^ Closure to decode.
        -> IO Closure
        -- ^ Heap representation of the closure.

instance HasHeapRep (a :: TYPE 'LiftedRep) where
    getClosureData = getClosureDataFromHeapObject

instance HasHeapRep (a :: TYPE 'UnliftedRep) where
    getClosureData x = getClosureDataFromHeapObject (unsafeCoerce# x)

instance Int# ~ a => HasHeapRep (a :: TYPE 'IntRep) where
    getClosureData x = return $
        IntClosure { ptipe = PInt, intVal = I# x }

instance Word# ~ a => HasHeapRep (a :: TYPE 'WordRep) where
    getClosureData x = return $
        WordClosure { ptipe = PWord, wordVal = W# x }

instance Int64# ~ a => HasHeapRep (a :: TYPE 'Int64Rep) where
    getClosureData x = return $
        Int64Closure { ptipe = PInt64, int64Val = I64# (unsafeCoerce# x) }

instance Word64# ~ a => HasHeapRep (a :: TYPE 'Word64Rep) where
    getClosureData x = return $
        Word64Closure { ptipe = PWord64, word64Val = W64# (unsafeCoerce# x) }

instance Addr# ~ a => HasHeapRep (a :: TYPE 'AddrRep) where
    getClosureData x = return $
        AddrClosure { ptipe = PAddr, addrVal = I# (unsafeCoerce# x) }

instance Float# ~ a => HasHeapRep (a :: TYPE 'FloatRep) where
    getClosureData x = return $
        FloatClosure { ptipe = PFloat, floatVal = F# x }

instance Double# ~ a => HasHeapRep (a :: TYPE 'DoubleRep) where
    getClosureData x = return $
        DoubleClosure { ptipe = PDouble, doubleVal = D# x }

-- | Get the heap representation of a closure _at this moment_, even if it is
-- unevaluated or an indirection or other exotic stuff. Beware when passing
-- something to this function, the same caveats as for
-- 'GHC.Exts.Heap.Closures.asBox' apply.
--
-- For most use cases 'getClosureData' is an easier to use alternative.
--
-- Currently TSO and STACK objects will return `UnsupportedClosure`. This is
-- because it is not memory safe to extract TSO and STACK objects (done via
-- `unpackClosure#`). Other threads may be mutating those objects and interleave
-- with reads in `unpackClosure#`. This is particularly problematic with STACKs
-- where pointer values may be overwritten by non-pointer values as the haskell
-- thread runs.
getClosureDataFromHeapObject
    :: a
    -- ^ Heap object to decode.
    -> IO Closure
    -- ^ Heap representation of the closure.
getClosureDataFromHeapObject x = do
    case unpackClosure# x of
#if MIN_VERSION_ghc_prim(0,5,3)
        (# infoTableAddr, heapRep, pointersArray #) -> do
#else
        -- This is a hack to cover the bootstrap compiler using the old version
        -- of 'unpackClosure'. The new 'unpackClosure' return values are not
        -- merely a reordering, so using the old version would not work.
        (# infoTableAddr, pointersArray, heapRep #) -> do
#endif
            let infoTablePtr = Ptr infoTableAddr
                ptrList = [case indexArray# pointersArray i of
                                (# ptr #) -> Box ptr
                            | I# i <- [0..(I# (sizeofArray# pointersArray)) - 1]
                            ]

            infoTable <- peekItbl infoTablePtr
            case tipe infoTable of
                TSO   -> pure $ UnsupportedClosure infoTable
                STACK -> pure $ UnsupportedClosure infoTable
                _ -> getClosureDataFromHeapRep Nothing heapRep infoTablePtr ptrList

-- | Convert an unpacked heap object, to a `GenClosure b`. The inputs to this
-- function can be generated from a heap object using `unpackClosure#`. Care
-- must be take to ensure that the closure is not moved by the GC between
-- calling `unpackClosure#` and reading the closure's address (the first
-- argument to `getClosureDataFromHeapRep`).
getClosureDataFromHeapRep
    :: Maybe (Ptr a)
    -- ^ Pointer to the closure in the heap. This is only used for STACK
    -- closures to properly calculate the `stack_spOffset`. If this argument is
    -- Nothing and the closure is a STACK, then `UnsupportedClosure` is
    -- returned.
    -> ByteArray#
    -- ^ Heap representation of the closure as returned by `unpackClosure#`.
    -- This includes all of the object including the header, info table
    -- pointer, pointer data, and non-pointer data. The ByteArray# may be
    -- pinned or unpinned.
    -> Ptr StgInfoTable
    -- ^ Pointer to the `StgInfoTable` of the closure, extracted from the heap
    -- representation. The info table must not be moved by GC i.e. must be an
    -- info table from this process's runtime or in pinned or off-heap memory.
    -> [b]
    -- ^ Pointers in the payload of the closure, extracted from the heap
    -- representation. In the case of STACK objects, this does NOT contain
    -- pointers in the stack space (i.e. in StgStack::stack). Note `b` is some
    -- representation of a pointer. If for example `b ~ Any` then the referenced
    -- objects will be managed by the runtime system and kept alive by the
    -- garbage collector. That is not true if for example `b ~ Ptr Any`.
    -> IO (GenClosure b)
    -- ^ Heap representation of the closure.
getClosureDataFromHeapRep closureAddressMay heapRep infoTablePtr pts = do
    itbl <- peekItbl infoTablePtr
    -- The remaining words after the header
    let -- heapRep as a list of words.
        rawHeapWords :: [Word]
        rawHeapWords = [W# (indexWordArray# heapRep i) | I# i <- [0.. end] ]
            where
            nelems = (I# (sizeofByteArray# heapRep)) `div` wORD_SIZE
            end = fromIntegral nelems - 1

        -- Just the payload of rawHeapWords (no header).
        payloadWords :: [Word]
        payloadWords = drop (closureTypeHeaderSize (tipe itbl)) rawHeapWords

        -- The non-pointer words in the payload. Only valid for closures with a
        -- "pointers first" layout. Not valid for bit field layout.
        npts :: [Word]
        npts = drop (closureTypeHeaderSize (tipe itbl) + length pts) rawHeapWords
    case tipe itbl of
        t | t >= CONSTR && t <= CONSTR_NOCAF -> do
            (p, m, n) <- dataConNames infoTablePtr
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
            unless (length payloadWords >= 2) $
                fail $ "Expected at least 2 raw words to AP"
            let splitWord = payloadWords !! 0
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
            unless (length payloadWords >= 2) $
                fail "Expected at least 2 raw words to PAP"
            let splitWord = payloadWords !! 0
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
            unless (length payloadWords >= 4) $
                fail $ "Expected at least 4 words to BCO, found "
                        ++ show (length payloadWords)
            let splitWord = payloadWords !! 3
            pure $ BCOClosure itbl (pts !! 0) (pts !! 1) (pts !! 2)
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (drop 4 payloadWords)

        ARR_WORDS -> do
            unless (length payloadWords >= 1) $
                fail $ "Expected at least 1 words to ARR_WORDS, found "
                        ++ show (length payloadWords)
            pure $ ArrWordsClosure itbl (head payloadWords) (tail payloadWords)

        t | t >= MUT_ARR_PTRS_CLEAN && t <= MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length payloadWords >= 2) $
                fail $ "Expected at least 2 words to MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)
            pure $ MutArrClosure itbl (payloadWords !! 0) (payloadWords !! 1) pts

        t | t >= SMALL_MUT_ARR_PTRS_CLEAN && t <= SMALL_MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length payloadWords >= 1) $
                fail $ "Expected at least 1 word to SMALL_MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)
            pure $ SmallMutArrClosure itbl (payloadWords !! 0) pts

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
            pure $ OtherClosure itbl pts rawHeapWords
        --    pure $ BlockingQueueClosure itbl
        --        (pts !! 0) (pts !! 1) (pts !! 2) (pts !! 3)

        --  pure $ OtherClosure itbl pts rawHeapWords
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
                -> withArray rawHeapWords (\ptr -> do
                    fields <- FFIClosures.peekTSOFields peekStgTSOProfInfo ptr
                    pure $ TSOClosure
                        { info = itbl
                        , link = u_lnk
                        , global_link = u_gbl_lnk
                        , tsoStack = tso_stack
                        , trec = u_trec
                        , blocked_exceptions = u_blk_ex
                        , bq = u_bq
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
        STACK
            | [] <- pts
            -> case closureAddressMay of
                Nothing -> pure $ UnsupportedClosure itbl
                Just (Ptr closureAddress) ->  withArray rawHeapWords (\ptr -> do
                            fields <- FFIClosures.peekStackFields ptr
                            let sp = FFIClosures.stack_sp fields
                                spOffset = I# (minusAddr# sp closureAddress)
                            pure $ StackClosure
                                { info = itbl
                                , stack_size = FFIClosures.stack_size fields
                                , stack_dirty = FFIClosures.stack_dirty fields
#if __GLASGOW_HASKELL__ >= 811
                                , stack_marking = FFIClosures.stack_marking fields
#endif
                                , stack_spOffset = spOffset
                                })
            | otherwise
                -> fail $ "Expected 0 ptr argument to STACK, found "
                    ++ show (length pts)

        _ ->
            pure $ UnsupportedClosure itbl

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a
