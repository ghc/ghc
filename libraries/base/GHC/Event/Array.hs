{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns, CPP, NoImplicitPrelude #-}

module GHC.Event.Array
    (
      Array
    , capacity
    , empty
    , ensureCapacity
    , findIndex
    , forM_
    , loop
    , new
    , removeAt
    , snoc
    , unsafeLoad
    , unsafeWrite
    , useAsPtr
    ) where

import Data.Bits ((.|.), shiftR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe
import GHC.Base hiding (empty)
import GHC.Num (Num(..))
import GHC.Ptr (Ptr(..))

import GHC.Primitive.PrimArray
import GHC.Primitive.Monad (Prim,sizeOf#)

#include "MachDeps.h"

-- Invariant: length <= capacity
--
-- All the documentation in this module refers to two size-like
-- properties of the array: length and capacity. The length is
-- the current number of active elements in the array. The capacity
-- is the maximum number of elements the array can currently hold.
--
-- The capacity is always a power of two. Is this actually
-- required by any of the backends?
newtype Array a = Array (IORef (AC a))

-- The actual array content. The type `a` is required
-- to have a size that is a multiple of the size of a machine word. We
-- use the first slot in the array to store the current
-- number of live elements. This is a hack, but it means
-- that we can cut down on allocations and indirections.
-- The reward gets even sweeter once
-- https://github.com/ghc-proposals/ghc-proposals/pull/203
-- happens, since that will allow us to put a
-- MutableByteArray# directly in a MutVar#.
--
-- The mutable prim array must be pinned. The Poll
-- backend (GHC/Event/Poll.hsc) calls the useAsPtr function
-- provided by this module. The other backends (epoll and
-- kqueue) use unsafeLoad, which also needs a pointer.
newtype AC a = AC (MutablePrimArray RealWorld a)

empty :: Prim a => IO (Array a)
empty = do
  p <- newPinnedPrimArray 1
  writeElementCount p 0
  fmap Array (newIORef (AC p))

new :: Prim a => Int -> IO (Array a)
new c = do
    p <- newPinnedPrimArray (cap + 1)
    writeElementCount p 0
    fmap Array (newIORef (AC p))
  where
    cap = firstPowerOf2 c

capacity :: Prim a => Array a -> IO Int
capacity (Array ref) = do
    AC p <- readIORef ref
    sz <- getSizeofMutablePrimArray p
    return (sz - 1)

-- | Write an element to the given index. This does not
--   check to see if the index is lower than the capacity.
unsafeWrite :: Prim a => Array a -> Int -> a -> IO ()
unsafeWrite (Array ref) ix a = do
    AC p <- readIORef ref
    unsafeWrite' p ix a

unsafeWrite' :: Prim a => MutablePrimArray RealWorld a -> Int -> a -> IO ()
unsafeWrite' p ix a = writePrimArray p (ix + 1) a

-- Run a callback that takes a Ptr to the contents and the
-- capacity. Replaces the length with the value returned
-- by the callback.
unsafeLoad :: Prim a
  => Array a
  -> (Ptr a -> Int -> IO Int) -- callback args are pointer and capacity
  -> IO Int
unsafeLoad (Array ref) load = do
    AC p <- readIORef ref
    sz <- getSizeofMutablePrimArray p
    let cap = sz - 1
    -- This pointer conversion is safe since the next
    -- instruction (writeElementCount) guarantees that
    -- the array will be live on the heap.
    len' <- load (advancePtr (mutablePrimArrayContents p) 1) cap
    writeElementCount p len'
    return len'

ensureCapacity :: Prim a => Array a -> Int -> IO ()
ensureCapacity (Array ref) c = do
    AC p <- readIORef ref
    sz <- getSizeofMutablePrimArray p
    let cap = sz - 1
    let cap' = firstPowerOf2 c
    if c > cap
      then do
        p' <- newPrimArray (cap' + 1)
        copyMutablePrimArray p' 0 p 0 sz
        writeIORef ref (AC p')
      else pure ()

-- Run a callback that takes a Ptr to the contents and the
-- length. Returns whatever the callback returned.
useAsPtr :: Prim a
  => Array a
  -> (Ptr a -> Int -> IO b) -- callback args are pointer and length
  -> IO b
useAsPtr (Array ref) f = do
    AC p <- readIORef ref
    len <- readElementCount p
    r <- f (advancePtr (mutablePrimArrayContents p) 1) len
    touchMutablePrimArray p
    return r

-- Push an element onto the end of the array. This function
-- ensures that enough space is available, growing the array
-- if neccessary.
snoc :: Prim a => Array a -> a -> IO ()
snoc (Array ref) e = do
    AC p <- readIORef ref
    sz <- getSizeofMutablePrimArray p
    len <- readElementCount p
    let cap = sz - 1
    let len' = len + 1
    let cap' = firstPowerOf2 len'
    if len' > cap
      then do
        p' <- newPrimArray (cap' + 1)
        copyMutablePrimArray p' 0 p 0 sz
        unsafeWrite' p' len e
        writeIORef ref (AC p')
      else unsafeWrite' p len e

-- Execute the provided action on each element in the array.
forM_ :: Prim a => Array a -> (a -> IO ()) -> IO ()
forM_ (Array ref) g = do
  AC p <- readIORef ref
  len <- readElementCount p
  -- This loop is a little unusual. Remember that the first element in
  -- the array is not an element at all. It is the length (the count of
  -- active elements). Consequently, we skip this element, and we go one
  -- element further than we normally would. Note the use of LTE rather
  -- than LT in the if expression.
  let go !ix = if ix <= len
        then do
          g =<< readPrimArray p ix
          go (ix + 1)
        else pure ()
  go 1

-- Traverse the array with some effectful callback until the
-- callback returns False. The callback also uses an accumulator.
-- This function is strict in the accumulator.
loop :: Prim a => Array a -> b -> (b -> a -> IO (b,Bool)) -> IO ()
loop (Array ref) z g = do
  AC p <- readIORef ref
  len <- readElementCount p
  -- This loop is a little unusual. See the comment in forM_.
  let go !ix !k = if ix <= len
        then do
          (k', cont) <- g k =<< readPrimArray p ix
          when cont (go (ix + 1) k')
        else pure ()
  go 1 z

-- Find the first occurence of the element. Returns the
-- element and its index.
findIndex :: Prim a => (a -> Bool) -> Array a -> IO (Maybe (Int,a))
findIndex predicate (Array ref) = do
  AC p <- readIORef ref
  len <- readElementCount p
  -- This loop is a little unusual. See the comment in forM_.
  let go !ix = if ix <= len
        then do
          val <- readPrimArray p ix
          if predicate val
            then pure (Just (ix,val))
            else go ix
        else pure Nothing
  go 1

-- Remove the element at the given index.
removeAt :: Prim a => Array a -> Int -> IO ()
removeAt (Array ref) i = do
  AC p <- readIORef ref
  oldLen <- readElementCount p
  when (i < 0 || i >= oldLen) $ errorWithoutStackTrace "removeAt: invalid index"
  let newLen = oldLen - 1
  writeElementCount p newLen
  copyMutablePrimArray p (i + 2) p (i + 1) (newLen - i)

-- Write a machine int (indicating the number of active elements)
-- to the first slot in the array. This ignores the element
-- type, which is expected to have a size that is a multiple of the
-- size of a machine int.
writeElementCount :: MutablePrimArray RealWorld a -> Int -> IO ()
writeElementCount (MutablePrimArray arr) i = writePrimArray
  (MutablePrimArray arr :: MutablePrimArray RealWorld Int) 0 i

readElementCount :: MutablePrimArray RealWorld a -> IO Int
readElementCount (MutablePrimArray arr) = readPrimArray
  (MutablePrimArray arr :: MutablePrimArray RealWorld Int) 0

-- Offset a pointer by the given number of elements. This uses
-- the Prim instances instead of the Storable instance.
advancePtr :: forall a. Prim a => Ptr a -> Int -> Ptr a
{-# INLINE advancePtr #-}
advancePtr (Ptr a#) (I# i#) = Ptr (plusAddr# a# (i# *# sizeOf# (undefined :: a)))


{-The firstPowerOf2 function works by setting all bits on the right-hand
side of the most significant flagged bit to 1, and then incrementing
the entire value at the end so it "rolls over" to the nearest power of
two.
-}

-- | Computes the next-highest power of two for a particular integer,
-- @n@.  If @n@ is already a power of two, returns @n@.  If @n@ is
-- zero, returns zero, even though zero is not a power of two.
firstPowerOf2 :: Int -> Int
firstPowerOf2 !n =
    let !n1 = n - 1
        !n2 = n1 .|. (n1 `shiftR` 1)
        !n3 = n2 .|. (n2 `shiftR` 2)
        !n4 = n3 .|. (n3 `shiftR` 4)
        !n5 = n4 .|. (n4 `shiftR` 8)
        !n6 = n5 .|. (n5 `shiftR` 16)
#if WORD_SIZE_IN_BITS == 32
    in n6 + 1
#elif WORD_SIZE_IN_BITS == 64
        !n7 = n6 .|. (n6 `shiftR` 32)
    in n7 + 1
#else
# error firstPowerOf2 not defined on this architecture
#endif
