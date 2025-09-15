-- This test verifies that correct (not out-of-bounds) uses
-- of primops that we can bounds-check with -fcheck-prim-bounds
-- do not cause spurious bounds-checking failures.

-- Currently this tests most ByteArray#, Array#, and SmallArray# operations.
-- (Theoretically it could also test Addr# operations,
-- since those /can/ be bounds-checked with the JS back-end.)

{-# LANGUAGE CPP #-}

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Array.Byte
import Data.Bits
import Control.Monad
import GHC.Exts
import GHC.IO
import GHC.Word
import GHC.Int
import GHC.Float
import GHC.Stable
import System.IO

#define TEST_READ_WRITE(CONDITION, READ_OP, INDEX_OP, WRITE_OP) \
  when (CONDITION) $ IO $ \s0 -> \
    case (READ_OP) arrU# i# s0 of \
      (# s1, v# #) -> case (WRITE_OP) arrP# i# v# s1 of \
        s2 -> (# (WRITE_OP) arrU# i# ((INDEX_OP) arrF# i#) s2, () #)

#define ALIGNED_RW(WIDTH, READ_OP, INDEX_OP, WRITE_OP) \
  TEST_READ_WRITE(i < size `div` (WIDTH), READ_OP, INDEX_OP, WRITE_OP)

#define UNALIGNED_RW(WIDTH, READ_OP, INDEX_OP, WRITE_OP) \
  TEST_READ_WRITE(i + (WIDTH) <= size, READ_OP, INDEX_OP, WRITE_OP)

#define TEST_CAS(WIDTH, CON, CAS_OP) \
  when (i < size `div` (WIDTH)) $ IO $ \s0 -> \
    case (0, 7) of \
      (CON v0, CON v7) -> case (CAS_OP) arrU# i# v0 v7 s0 of \
        (# s1, v' #) -> (# s1, () #)


wrapEffect :: (State# RealWorld -> State# RealWorld) -> IO ()
wrapEffect eff = IO (\s0 -> (# eff s0, () #))


testByteArraysOfSize :: Int -> IO ()
testByteArraysOfSize (size@(I# size#)) = do
  let mkArr op = IO $ \s0 -> case op size# s0 of
        (# s1, newArr #)
          -> (# setByteArray# newArr 0# size# 123# s1,
                MutableByteArray newArr #)
  MutableByteArray arrU# <- mkArr newByteArray#
  MutableByteArray arrP# <- mkArr newPinnedByteArray#
  ByteArray arrF# <- do
    MutableByteArray arrToFreeze <- mkArr newByteArray#
    IO $ \s0 -> case unsafeFreezeByteArray# arrToFreeze s0 of
      (# s1, frozenArr #) -> (# s1, ByteArray frozenArr #)
  let !nws = finiteBitSize (0 :: Int) `div` 8
      !bufP = mutableByteArrayContents# arrP#


  forM_ [0..size] $ \i@(I# i#) -> do
    -- test valid aligned read/write ops
    -- (expressed via CPP macro because of non-uniform representations)
    ALIGNED_RW(1, readWord8Array#, indexWord8Array#, writeWord8Array#)
    ALIGNED_RW(2, readWord16Array#, indexWord16Array#, writeWord16Array#)
    ALIGNED_RW(4, readWord32Array#, indexWord32Array#, writeWord32Array#)
    ALIGNED_RW(8, readWord64Array#, indexWord64Array#, writeWord64Array#)
    ALIGNED_RW(nws, readWordArray#, indexWordArray#, writeWordArray#)

    ALIGNED_RW(1, readInt8Array#, indexInt8Array#, writeInt8Array#)
    ALIGNED_RW(2, readInt16Array#, indexInt16Array#, writeInt16Array#)
    ALIGNED_RW(4, readInt32Array#, indexInt32Array#, writeInt32Array#)
    ALIGNED_RW(8, readInt64Array#, indexInt64Array#, writeInt64Array#)
    ALIGNED_RW(nws, readIntArray#, indexIntArray#, writeIntArray#)

    ALIGNED_RW(4, readFloatArray#, indexFloatArray#, writeFloatArray#)
    ALIGNED_RW(8, readDoubleArray#, indexDoubleArray#, writeDoubleArray#)

    ALIGNED_RW(1, readCharArray#, indexCharArray#, writeCharArray#)
    ALIGNED_RW(4, readWideCharArray#, indexWideCharArray#, writeWideCharArray#)

    -- TODO: What is the right condition is for Addr# with the JS backend?
    ALIGNED_RW(nws, readAddrArray#, indexAddrArray#, writeAddrArray#)
    ALIGNED_RW(nws, readStablePtrArray#, indexStablePtrArray#, writeStablePtrArray#)


    -- test valid unaligned read/write ops
    -- (expressed via CPP macro because of non-uniform representations)
    -- no primops for unaligned word8 access
    UNALIGNED_RW(2, readWord8ArrayAsWord16#, indexWord8ArrayAsWord16#, writeWord8ArrayAsWord16#)
    UNALIGNED_RW(4, readWord8ArrayAsWord32#, indexWord8ArrayAsWord32#, writeWord8ArrayAsWord32#)
    UNALIGNED_RW(8, readWord8ArrayAsWord64#, indexWord8ArrayAsWord64#, writeWord8ArrayAsWord64#)
    UNALIGNED_RW(nws, readWord8ArrayAsWord#, indexWord8ArrayAsWord#, writeWord8ArrayAsWord#)

    -- no primops for unaligned int8 access
    UNALIGNED_RW(2, readWord8ArrayAsInt16#, indexWord8ArrayAsInt16#, writeWord8ArrayAsInt16#)
    UNALIGNED_RW(4, readWord8ArrayAsInt32#, indexWord8ArrayAsInt32#, writeWord8ArrayAsInt32#)
    UNALIGNED_RW(8, readWord8ArrayAsInt64#, indexWord8ArrayAsInt64#, writeWord8ArrayAsInt64#)
    UNALIGNED_RW(nws, readWord8ArrayAsInt#, indexWord8ArrayAsInt#, writeWord8ArrayAsInt#)

    UNALIGNED_RW(4, readWord8ArrayAsFloat#, indexWord8ArrayAsFloat#, writeWord8ArrayAsFloat#)
    UNALIGNED_RW(8, readWord8ArrayAsDouble#, indexWord8ArrayAsDouble#, writeWord8ArrayAsDouble#)

    UNALIGNED_RW(1, readWord8ArrayAsChar#, indexWord8ArrayAsChar#, writeWord8ArrayAsChar#)
    UNALIGNED_RW(4, readWord8ArrayAsWideChar#, indexWord8ArrayAsWideChar#, writeWord8ArrayAsWideChar#)

    -- TODO: What is the right condition is for Addr# with the JS backend?
    UNALIGNED_RW(nws, readWord8ArrayAsAddr#, indexWord8ArrayAsAddr#, writeWord8ArrayAsAddr#)
    UNALIGNED_RW(nws, readWord8ArrayAsStablePtr#, indexWord8ArrayAsStablePtr#, writeWord8ArrayAsStablePtr#)


    when (i < size `div` nws) $ do
      let testFetchModify :: (MutableByteArray# RealWorld -> Int# -> Int#
                          -> State# RealWorld -> (# State# RealWorld, Int# #))
                          -> IO ()
          testFetchModify op
            = IO (\s -> case op arrU# i# 137# s of (# s', _ #) -> (# s', () #) )
      testFetchModify fetchXorIntArray#
      testFetchModify fetchOrIntArray#
      testFetchModify fetchNandIntArray#
      testFetchModify fetchAndIntArray#
      testFetchModify fetchSubIntArray#
      testFetchModify fetchAddIntArray#

      IO $ \s0 -> case atomicReadIntArray# arrU# i# s0 of
        (# s1, v #) -> (# atomicWriteIntArray# arrP# i# v s1, () #)


    TEST_CAS(8, I64#, casInt64Array#)
    TEST_CAS(4, I32#, casInt32Array#)
    TEST_CAS(2, I16#, casInt16Array#)
    TEST_CAS(1, I8# , casInt8Array#)
    TEST_CAS(nws, I#, casIntArray#)


  -- test valid range ops
  forM_ [0..size] $ \rangeLen@(I# rangeLen#) -> do
    let ixs | rangeLen == 0 = [-4 .. size + 4] -- empty ranges are not out-of-bounds
            | otherwise     = [0 .. size - rangeLen]
    forM_ ixs $ \i@(I# i#) -> do
      wrapEffect (setByteArray# arrU# i# rangeLen# 234#)
      forM_ ixs $ \j@(I# j#) -> do
        wrapEffect (copyMutableByteArrayNonOverlapping# arrP# i# arrU# j# rangeLen#)
        wrapEffect (copyByteArray# arrF# i# arrU# j# rangeLen#)
        wrapEffect (copyMutableByteArray# arrU# i# arrP# j# rangeLen#)
        wrapEffect (copyMutableByteArray# arrU# i# arrU# j# rangeLen#)
        case compareByteArrays# arrF# i# arrF# j# rangeLen# of
          v -> wrapEffect (setByteArray# arrP# j# rangeLen# (v `andI#` 255#))
        let !rangeP = bufP `plusAddr#` j#
        wrapEffect (copyAddrToByteArray# rangeP arrU# i# rangeLen#)
        wrapEffect (copyMutableByteArrayToAddr# arrU# i# rangeP rangeLen#)
        wrapEffect (copyByteArrayToAddr# arrF# i# rangeP rangeLen#)
        when (abs (i - j) >= rangeLen) $
          wrapEffect (copyMutableByteArrayNonOverlapping# arrU# i# arrU# j# rangeLen#)



data Array a = Array (Array# a)
data MutableArray s a = MutableArray (MutableArray# s a)
data SmallArray a = SmallArray (SmallArray# a)
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)


testArraysOfSize :: Int -> IO ()
testArraysOfSize (size@(I# size#)) = do
  let mkArr v = IO $ \s0 -> case newArray# size# v s0 of
        (# s1, newArr #) -> (# s1, MutableArray newArr #)
  MutableArray arrM# <- mkArr 0
  Array arrF# <- do
    MutableArray arrToFreeze <- mkArr 0
    forM_ [0 .. size - 1] $ \(i@(I# i#)) -> do
      wrapEffect (writeArray# arrM# i# i)
      wrapEffect (writeArray# arrToFreeze i# i)

    IO $ \s0 -> case unsafeFreezeArray# arrToFreeze s0 of
      (# s1, frozenArr #) -> (# s1, Array frozenArr #)

  forM_ [0 .. size - 1] $ \(i@(I# i#)) -> do

    -- test read/index/write
    IO $ \s0 -> case readArray# arrM# i# s0 of
      (# s1, vm #) -> case indexArray# arrF# i# of
        (# vf #) -> (# writeArray# arrM# i# (vm + vf) s1, () #)

    -- test casArray
    IO $ \s0 -> case casArray# arrM# i# 0 7 s0 of
      (# s1, _, _ #) -> (# s1, () #)

  -- test valid range ops
  forM_ [0..size] $ \rangeLen@(I# rangeLen#) -> do
    let ixs | rangeLen == 0 = [-4 .. size + 4] -- empty ranges are not out-of-bounds
            | otherwise     = [0 .. size - rangeLen]
    forM_ ixs $ \(i@(I# i#)) -> do
      forM_ ixs $ \(j@(I# j#)) -> do
        wrapEffect (copyArray# arrF# i# arrM# j# rangeLen#)
        wrapEffect (copyMutableArray# arrM# i# arrM# j# rangeLen#)


testSmallArraysOfSize :: Int -> IO ()
testSmallArraysOfSize (size@(I# size#)) = do
  let mkArr v = IO $ \s0 -> case newSmallArray# size# v s0 of
        (# s1, newArr #) -> (# s1, SmallMutableArray newArr #)
  SmallMutableArray arrM# <- mkArr 0
  SmallArray arrF# <- do
    SmallMutableArray arrToFreeze <- mkArr 0
    forM_ [0 .. size - 1] $ \(i@(I# i#)) -> do
      wrapEffect (writeSmallArray# arrM# i# i)
      wrapEffect (writeSmallArray# arrToFreeze i# i)

    IO $ \s0 -> case unsafeFreezeSmallArray# arrToFreeze s0 of
      (# s1, frozenArr #) -> (# s1, SmallArray frozenArr #)

  forM_ [0 .. size - 1] $ \(i@(I# i#)) -> do

    -- test read/index/write
    IO $ \s0 -> case readSmallArray# arrM# i# s0 of
      (# s1, vm #) -> case indexSmallArray# arrF# i# of
        (# vf #) -> (# writeSmallArray# arrM# i# (vm + vf) s1, () #)

    -- test casSmallArray
    IO $ \s0 -> case casSmallArray# arrM# i# 0 7 s0 of
      (# s1, _, _ #) -> (# s1, () #)

  -- test valid range ops
  forM_ [0..size] $ \rangeLen@(I# rangeLen#) -> do
    let ixs | rangeLen == 0 = [-4 .. size + 4] -- empty ranges are not out-of-bounds
            | otherwise     = [0 .. size - rangeLen]
    forM_ ixs $ \(i@(I# i#)) -> do
      forM_ ixs $ \(j@(I# j#)) -> do
        wrapEffect (copySmallArray# arrF# i# arrM# j# rangeLen#)
        wrapEffect (copySmallMutableArray# arrM# i# arrM# j# rangeLen#)


main :: IO ()
main = forM_ ([0..4] ++ [24..32]) $ \size -> do
  testByteArraysOfSize size
  testArraysOfSize size
  testSmallArraysOfSize size
