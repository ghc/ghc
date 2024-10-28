{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
    , GenClosure(..)
    , ClosureType(..)
    , PrimType(..)
    , WhatNext(..)
    , WhyBlocked(..)
    , TsoFlags(..)
    , HasHeapRep(getClosureData)
    , getClosureInfoTbl
    , getClosureInfoTbl_maybe
    , getClosurePtrArgs
    , getClosurePtrArgs_maybe
    , getClosureDataFromHeapRep
    , getClosureDataFromHeapRepPrim

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
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
#endif
import GHC.Exts.Heap.Utils
import qualified GHC.Exts.Heap.FFIClosures as FFIClosures
import qualified GHC.Exts.Heap.ProfInfo.PeekProfInfo as PPI

import Data.Bits
import Foreign
import GHC.Exts
import GHC.Int
import GHC.Word

#include "ghcconfig.h"

class HasHeapRep (a :: TYPE rep) where

    -- | Decode a closure to it's heap representation ('GenClosure').
    getClosureData
        :: a
        -- ^ Closure to decode.
        -> IO Closure
        -- ^ Heap representation of the closure.

#if __GLASGOW_HASKELL__ >= 901
instance HasHeapRep (a :: TYPE ('BoxedRep 'Lifted)) where
#else
instance HasHeapRep (a :: TYPE 'LiftedRep) where
#endif
    getClosureData = getClosureDataFromHeapObject

#if __GLASGOW_HASKELL__ >= 901
instance HasHeapRep (a :: TYPE ('BoxedRep 'Unlifted)) where
#else
instance HasHeapRep (a :: TYPE 'UnliftedRep) where
#endif
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
        AddrClosure { ptipe = PAddr, addrVal = Ptr x }

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
-- where pointer values may be overwritten by non-pointer values as the
-- corresponding haskell thread runs.
getClosureDataFromHeapObject
    :: a
    -- ^ Heap object to decode.
    -> IO Closure
    -- ^ Heap representation of the closure.
getClosureDataFromHeapObject x = do
    case unpackClosure# x of
        (# infoTableAddr, heapRep, pointersArray #) -> do
            let infoTablePtr = Ptr infoTableAddr
                ptrList = [case indexArray# pointersArray i of
                                (# ptr #) -> Box ptr
                            | I# i <- [0..I# (sizeofArray# pointersArray) - 1]
                            ]

            infoTable <- peekItbl infoTablePtr
            case tipe infoTable of
                TSO   -> pure $ UnsupportedClosure infoTable
                STACK -> pure $ UnsupportedClosure infoTable
                _ -> getClosureDataFromHeapRep heapRep infoTablePtr ptrList


-- | Convert an unpacked heap object, to a `GenClosure b`. The inputs to this
-- function can be generated from a heap object using `unpackClosure#`.
getClosureDataFromHeapRep :: ByteArray# -> Ptr StgInfoTable -> [b] -> IO (GenClosure b)
getClosureDataFromHeapRep heapRep infoTablePtr pts = do
  itbl <- peekItbl infoTablePtr
  getClosureDataFromHeapRepPrim (dataConNames infoTablePtr) PPI.peekTopCCS itbl heapRep pts

getClosureDataFromHeapRepPrim
    :: IO (String, String, String)
    -- ^ A continuation used to decode the constructor description field,
    -- in ghc-debug this code can lead to segfaults because dataConNames
    -- will dereference a random part of memory.
    -> (Ptr a -> IO (Maybe CostCentreStack))
    -- ^ A continuation which is used to decode a cost centre stack
    -- In ghc-debug, this code will need to call back into the debuggee to
    -- fetch the representation of the CCS before decoding it. Using
    -- `peekTopCCS` for this argument can lead to segfaults in ghc-debug as
    -- the CCS argument will point outside the copied closure.
    -> StgInfoTable
    -- ^ The `StgInfoTable` of the closure, extracted from the heap
    -- representation.
    -> ByteArray#
    -- ^ Heap representation of the closure as returned by `unpackClosure#`.
    -- This includes all of the object including the header, info table
    -- pointer, pointer data, and non-pointer data. The ByteArray# may be
    -- pinned or unpinned.
    -> [b]
    -- ^ Pointers in the payload of the closure, extracted from the heap
    -- representation as returned by `collect_pointers()` in `Heap.c`. The type
    -- `b` is some representation of a pointer e.g. `Any` or `Ptr Any`.
    -> IO (GenClosure b)
    -- ^ Heap representation of the closure.
getClosureDataFromHeapRepPrim getConDesc decodeCCS itbl heapRep pts = do
    let -- heapRep as a list of words.
        rawHeapWords :: [Word]
        rawHeapWords = [W# (indexWordArray# heapRep i) | I# i <- [0.. end] ]
            where
            nelems = I# (sizeofByteArray# heapRep) `div` wORD_SIZE
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
            (p, m, n) <- getConDesc
            pure $ ConstrClosure itbl pts npts p m n

        t | t >= THUNK && t <= THUNK_STATIC -> do
            pure $ ThunkClosure itbl pts npts

        THUNK_SELECTOR -> case pts of
            [] -> fail "Expected at least 1 ptr argument to THUNK_SELECTOR"
            hd : _ -> pure $ SelectorClosure itbl hd

        t | t >= FUN && t <= FUN_STATIC -> do
            pure $ FunClosure itbl pts npts

        AP -> case pts of
            [] -> fail "Expected at least 1 ptr argument to AP"
            hd : tl -> case payloadWords of
                -- We expect at least the arity, n_args, and fun fields
                splitWord : _ : _ ->
                    pure $ APClosure itbl
#if defined(WORDS_BIGENDIAN)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                        (fromIntegral splitWord)
#else
                        (fromIntegral splitWord)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                        hd tl
                _ -> fail "Expected at least 2 raw words to AP"

        PAP -> case pts of
            [] -> fail "Expected at least 1 ptr argument to PAP"
            hd : tl -> case payloadWords of
                -- We expect at least the arity, n_args, and fun fields
                splitWord : _ : _ ->
                    pure $ PAPClosure itbl
#if defined(WORDS_BIGENDIAN)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                        (fromIntegral splitWord)
#else
                        (fromIntegral splitWord)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                        hd tl
                _ -> fail "Expected at least 2 raw words to PAP"

        AP_STACK -> case pts of
            [] -> fail "Expected at least 1 ptr argument to AP_STACK"
            hd : tl -> pure $ APStackClosure itbl hd tl

        IND -> case pts of
            [] -> fail "Expected at least 1 ptr argument to IND"
            hd : _ -> pure $ IndClosure itbl hd

        IND_STATIC -> case pts of
            [] -> fail "Expected at least 1 ptr argument to IND_STATIC"
            hd : _ -> pure $ IndClosure itbl hd

        BLACKHOLE -> case pts of
            [] -> fail "Expected at least 1 ptr argument to BLACKHOLE"
            hd : _ -> pure $ BlackholeClosure itbl hd

        BCO -> case pts of
            pts0 : pts1 : pts2 : _ -> case payloadWords of
                _ : _ : _ : splitWord : payloadRest ->
                    pure $ BCOClosure itbl pts0 pts1 pts2
#if defined(WORDS_BIGENDIAN)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                        (fromIntegral splitWord)
#else
                        (fromIntegral splitWord)
                        (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                        payloadRest
                _ -> fail $ "Expected at least 4 words to BCO, found "
                            ++ show (length payloadWords)
            _ -> fail $ "Expected at least 3 ptr argument to BCO, found "
                        ++ show (length pts)

        ARR_WORDS -> case payloadWords of
            [] -> fail $ "Expected at least 1 words to ARR_WORDS, found "
                        ++ show (length payloadWords)
            hd : tl -> pure $ ArrWordsClosure itbl hd tl

        t | t >= MUT_ARR_PTRS_CLEAN && t <= MUT_ARR_PTRS_FROZEN_CLEAN -> case payloadWords of
            p0 : p1 : _ -> pure $ MutArrClosure itbl p0 p1 pts
            _ -> fail $ "Expected at least 2 words to MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)

        t | t >= SMALL_MUT_ARR_PTRS_CLEAN && t <= SMALL_MUT_ARR_PTRS_FROZEN_CLEAN -> case payloadWords of
            [] -> fail $ "Expected at least 1 word to SMALL_MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)
            hd : _ -> pure $ SmallMutArrClosure itbl hd pts

        t | t == MUT_VAR_CLEAN || t == MUT_VAR_DIRTY -> case pts of
            [] -> fail $ "Expected at least 1 words to MUT_VAR, found "
                        ++ show (length pts)
            hd : _ -> pure $ MutVarClosure itbl hd

        t | t == MVAR_CLEAN || t == MVAR_DIRTY -> case pts of
            pts0 : pts1 : pts2 : _ -> pure $ MVarClosure itbl pts0 pts1 pts2
            _ -> fail $ "Expected at least 3 ptrs to MVAR, found "
                        ++ show (length pts)

        BLOCKING_QUEUE
          | [_link, bh, _owner, msg] <- pts ->
            pure $ BlockingQueueClosure itbl _link bh _owner msg

        WEAK -> case pts of
            pts0 : pts1 : pts2 : pts3 : rest -> pure $ WeakClosure
                { info = itbl
                , cfinalizers = pts0
                , key = pts1
                , value = pts2
                , finalizer = pts3
                , weakLink = case rest of
                           []  -> Nothing
                           [p] -> Just p
                           _   -> error $ "Expected 4 or 5 words in WEAK, but found more: " ++ show (length pts)
                }
            _ -> error $ "Expected 4 or 5 words in WEAK, but found less: " ++ show (length pts)
        TSO | ( u_lnk : u_gbl_lnk : tso_stack : u_trec : u_blk_ex : u_bq : other)  <- pts
                -> withArray rawHeapWords (\ptr -> do
                    fields <- FFIClosures.peekTSOFields decodeCCS ptr
                    pure $ TSOClosure
                        { info = itbl
                        , link = u_lnk
                        , global_link = u_gbl_lnk
                        , tsoStack = tso_stack
                        , trec = u_trec
                        , blocked_exceptions = u_blk_ex
                        , bq = u_bq
                        , thread_label = case other of
                                          [tl] -> Just tl
                                          [] -> Nothing
                                          _ -> error $ "thead_label:Expected 0 or 1 extra arguments"
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
                -> fail $ "Expected at least 6 ptr arguments to TSO, found "
                        ++ show (length pts)
        STACK
            | [] <- pts
            -> withArray rawHeapWords (\ptr -> do
                            fields <- FFIClosures.peekStackFields ptr
                            pure $ StackClosure
                                { info = itbl
                                , stack_size = FFIClosures.stack_size fields
                                , stack_dirty = FFIClosures.stack_dirty fields
#if __GLASGOW_HASKELL__ >= 811
                                , stack_marking = FFIClosures.stack_marking fields
#endif
                                })
            | otherwise
                -> fail $ "Expected 0 ptr argument to STACK, found "
                    ++ show (length pts)

        _ ->
            pure $ UnsupportedClosure itbl

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a
