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
  , readFirstFastMutInt
  , readSecondFastMutInt
  , writeFirstFastMutInt
  , writeSecondFastMutInt
  , atomicFetchAddFirstFastMut
  , atomicFetchAddSecondFastMut
  , readFastMutInt
  , writeFastMutInt
  )
where

import GHC.Prelude.Basic

import GHC.Base

data FastMutInts (n :: VarCount) = FastMutInts !(MutableByteArray# RealWorld)
-- It's likely possible to generalise this to n-variables, but no
-- use cases exist so far in GHC, so we currently choose the simplicity
-- of implementation.
data VarCount = OneVar | TwoVars

type FastMutInt  = FastMutInts 'OneVar
type FastMutInt2 = FastMutInts 'TwoVars


-- Keep the old names around for Haddock:
readFastMutInt :: FastMutInt -> IO Int
readFastMutInt = readFirstFastMutInt

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt = writeFirstFastMutInt


-- | Allocate a single mutable int with an initial value.
newFastMutInt :: Int -> IO FastMutInt
{-# INLINE newFastMutInt #-}
newFastMutInt n = do
    let size = finiteBitSize (0 :: Int) `unsafeShiftR` 3
    x <- createFastMutInt size
    writeFirstFastMutInt x n
    return x

-- | Allocate a pair of mutable ints with initial values.
newFastMutInt2 :: Int -> Int -> IO FastMutInt2
{-# INLINE newFastMutInt2 #-}
newFastMutInt2 n0 n1 = do
    let size = finiteBitSize (0 :: Int) `unsafeShiftR` 2
                  -- only "shiftR 2" to account for "times 2"
    x <- createFastMutInt size
    writeFirstFastMutInt x n0
    writeSecondFastMutInt x n1
    return x

-- | Allocate space for n mutable ints.
createFastMutInt :: Int -> IO (FastMutInts n)
{-# INLINE createFastMutInt #-}
createFastMutInt (I# size) = IO $ \s ->
  case newByteArray# size s of
    (# s, arr #) -> (# s, FastMutInts arr #)

-- | Read the first int from either a single or pair of
-- mutable ints.
readFirstFastMutInt :: FastMutInts n -> IO Int
{-# INLINE readFirstFastMutInt #-}
readFirstFastMutInt (FastMutInts arr) = IO $ \s ->
  case readIntArray# arr 0# s of
    (# s, i #) -> (# s, I# i #)

-- | Read the second int from a pair of mutable ints.
readSecondFastMutInt :: FastMutInt2 -> IO Int
{-# INLINE readSecondFastMutInt #-}
readSecondFastMutInt (FastMutInts arr) = IO $ \s ->
  case readIntArray# arr 1# s of
    (# s, i #) -> (# s, I# i #)

-- | Write to a single mutable int, or the first slot of
-- a pair of mutable ints.
writeFirstFastMutInt :: FastMutInts n -> Int -> IO ()
{-# INLINE writeFirstFastMutInt #-}
writeFirstFastMutInt (FastMutInts arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of
    s -> (# s, () #)

-- | Write to the second slot of a pair of mutable ints.
writeSecondFastMutInt :: FastMutInt2 -> Int -> IO ()
{-# INLINE writeSecondFastMutInt #-}
writeSecondFastMutInt (FastMutInts arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 1# i s of
    s -> (# s, () #)

-- | Atomically modify a single mutable int, or the first slot
-- of a pair of mutable ints, by the given value.
atomicFetchAddFirstFastMut :: FastMutInts n -> Int -> IO Int
{-# INLINE atomicFetchAddFirstFastMut #-}
atomicFetchAddFirstFastMut (FastMutInts arr) (I# i) = IO $ \s ->
  case fetchAddIntArray# arr 0# i s of
    (# s, n #) -> (# s, I# n #)

-- | Atomically modify the second slot of a pair of mutable ints
-- by the given value.
atomicFetchAddSecondFastMut :: FastMutInt2 -> Int -> IO Int
{-# INLINE atomicFetchAddSecondFastMut #-}
atomicFetchAddSecondFastMut (FastMutInts arr) (I# i) = IO $ \s ->
  case fetchAddIntArray# arr 1# i s of
    (# s, n #) -> (# s, I# n #)
