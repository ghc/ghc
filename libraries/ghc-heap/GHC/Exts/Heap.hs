{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}

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
    , HasHeapRep(getClosureData)

    -- * Info Table types
    , StgInfoTable(..)
    , EntryFunPtr
    , HalfWord
    , ItblCodes
    , itblSize
    , peekItbl
    , pokeItbl

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
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
#endif
import GHC.Exts.Heap.Utils

import Control.Monad
import Data.Bits
import GHC.Arr
import GHC.Exts
import GHC.Int
import GHC.Word

#include "ghcconfig.h"

class HasHeapRep (a :: TYPE rep) where
    getClosureData :: a -> IO Closure

instance HasHeapRep (a :: TYPE 'LiftedRep) where
    getClosureData = getClosure

instance HasHeapRep (a :: TYPE 'UnliftedRep) where
    getClosureData x = getClosure (unsafeCoerce# x)

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

-- | This returns the raw representation of the given argument. The second
-- component of the triple is the raw words of the closure on the heap, and the
-- third component is those words that are actually pointers. Once back in the
-- Haskell world, the raw words that hold pointers may be outdated after a
-- garbage collector run, but the corresponding values in 'Box's will still
-- point to the correct value.
getClosureRaw :: a -> IO (Ptr StgInfoTable, [Word], [Box])
getClosureRaw x = do
    case unpackClosure# x of
-- This is a hack to cover the bootstrap compiler using the old version of
-- 'unpackClosure'. The new 'unpackClosure' return values are not merely
-- a reordering, so using the old version would not work.
        (# iptr, dat, pointers #) -> do
            let nelems = (I# (sizeofByteArray# dat)) `div` wORD_SIZE
                end = fromIntegral nelems - 1
                rawWds = [W# (indexWordArray# dat i) | I# i <- [0.. end] ]
                pelems = I# (sizeofArray# pointers)
                ptrList = amap' Box $ Array 0 (pelems - 1) pelems pointers
            pure (Ptr iptr, rawWds, ptrList)

-- From GHC.Runtime.Heap.Inspect
amap' :: (t -> b) -> Array Int t -> [b]
amap' f (Array i0 i _ arr#) = map g [0 .. i - i0]
    where g (I# i#) = case indexArray# arr# i# of
                          (# e #) -> f e

-- | This function returns a parsed heap representation of the argument _at
-- this moment_, even if it is unevaluated or an indirection or other exotic
-- stuff.  Beware when passing something to this function, the same caveats as
-- for 'asBox' apply.
getClosure :: a -> IO Closure
getClosure x = do
    (iptr, wds, pts) <- getClosureRaw x
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

        t | t == MUT_VAR_CLEAN || t == MUT_VAR_DIRTY ->
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

        _ ->
            pure $ UnsupportedClosure itbl

-- | Like 'getClosureData', but taking a 'Box', so it is easier to work with.
getBoxedClosureData :: Box -> IO Closure
getBoxedClosureData (Box a) = getClosureData a
