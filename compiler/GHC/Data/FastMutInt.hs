{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

-- (c) The University of Glasgow 2002-2006

-- | Unboxed mutable Ints
module GHC.Data.FastMutInt
  ( FastMutInt
  , FastMutInt2
  , newFastMutInt
  , newFastMutInt2
  , readFastMutInt
  , readFastMutInt2
  , writeFastMutInt
  , writeFastMutInt2
  , atomicFetchAddFastMut
  , atomicFetchAddFastMut2
  )
where

import GHC.Prelude.Basic

import GHC.Base

data FastMutInts (n :: S) = FastMutInts !(MutableByteArray# RealWorld)
data S = S1 | S2

type FastMutInt  = FastMutInts 'S1
type FastMutInt2 = FastMutInts 'S2

newFastMutInt :: Int -> IO FastMutInt
{-# INLINE newFastMutInt #-}
newFastMutInt n = do
    let size = finiteBitSize (0 :: Int) `unsafeShiftR` 3
    x <- createFastMutInt size
    writeFastMutInt x n
    return x

newFastMutInt2 :: Int -> Int -> IO FastMutInt2
{-# INLINE newFastMutInt2 #-}
newFastMutInt2 n0 n1 = do
    let size = finiteBitSize (0 :: Int) `unsafeShiftR` 2
                  -- only "shiftR 2" to account for "times 2"
    x <- createFastMutInt size
    writeFastMutInt x n0
    writeFastMutInt2 x n1
    return x

createFastMutInt :: Int -> IO (FastMutInts n)
{-# INLINE createFastMutInt #-}
createFastMutInt (I# size) = IO $ \s ->
  case newByteArray# size s of
    (# s, arr #) -> (# s, FastMutInts arr #)

readFastMutInt :: FastMutInts n -> IO Int
{-# INLINE readFastMutInt #-}
readFastMutInt (FastMutInts arr) = IO $ \s ->
  case readIntArray# arr 0# s of
    (# s, i #) -> (# s, I# i #)

readFastMutInt2 :: FastMutInt2 -> IO Int
{-# INLINE readFastMutInt2 #-}
readFastMutInt2 (FastMutInts arr) = IO $ \s ->
  case readIntArray# arr 1# s of
    (# s, i #) -> (# s, I# i #)

writeFastMutInt :: FastMutInts n -> Int -> IO ()
{-# INLINE writeFastMutInt #-}
writeFastMutInt (FastMutInts arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of
    s -> (# s, () #)

writeFastMutInt2 :: FastMutInt2 -> Int -> IO ()
{-# INLINE writeFastMutInt2 #-}
writeFastMutInt2 (FastMutInts arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 1# i s of
    s -> (# s, () #)

atomicFetchAddFastMut :: FastMutInts n -> Int -> IO Int
{-# INLINE atomicFetchAddFastMut #-}
atomicFetchAddFastMut (FastMutInts arr) (I# i) = IO $ \s ->
  case fetchAddIntArray# arr 0# i s of
    (# s, n #) -> (# s, I# n #)

atomicFetchAddFastMut2 :: FastMutInt2 -> Int -> IO Int
{-# INLINE atomicFetchAddFastMut2 #-}
atomicFetchAddFastMut2 (FastMutInts arr) (I# i) = IO $ \s ->
  case fetchAddIntArray# arr 1# i s of
    (# s, n #) -> (# s, I# n #)
