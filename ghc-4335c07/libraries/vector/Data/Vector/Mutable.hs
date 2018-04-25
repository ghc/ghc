{-# LANGUAGE CPP, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, BangPatterns, TypeFamilies #-}

-- |
-- Module      : Data.Vector.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable boxed vectors.
--

module Data.Vector.Mutable (
  -- * Mutable boxed vectors
  MVector(..), IOVector, STVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, modify, swap,
  unsafeRead, unsafeWrite, unsafeModify, unsafeSwap,

  -- * Modifying vectors

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove
) where

import           Control.Monad (when)
import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.Array
import           Control.Monad.Primitive

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail )

import Data.Typeable ( Typeable )

#include "vector.h"

-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(MutableArray s a)
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

-- NOTE: This seems unsafe, see http://trac.haskell.org/vector/ticket/54
{-
instance NFData a => NFData (MVector s a) where
    rnf (MVector i n arr) = unsafeInlineST $ force i
        where
          force !ix | ix < n    = do x <- readArray arr ix
                                     rnf x `seq` force (ix+1)
                    | otherwise = return ()
-}

instance G.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength (MVector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector i _ arr) = MVector (i+j) m arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    = do
        arr <- newArray n uninitialised
        return (MVector 0 n arr)

  {-# INLINE basicInitialize #-}
  -- initialization is unnecessary for boxed vectors
  basicInitialize _ = return ()

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x
    = do
        arr <- newArray n x
        return (MVector 0 n arr)

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ arr) j = readArray arr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ arr) j x = writeArray arr (i+j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableArray dst i src j n

  basicUnsafeMove dst@(MVector iDst n arrDst) src@(MVector iSrc _ arrSrc)
    = case n of
        0 -> return ()
        1 -> readArray arrSrc iSrc >>= writeArray arrDst iDst
        2 -> do
               x <- readArray arrSrc iSrc
               y <- readArray arrSrc (iSrc + 1)
               writeArray arrDst iDst x
               writeArray arrDst (iDst + 1) y
        _
          | overlaps dst src
             -> case compare iDst iSrc of
                  LT -> moveBackwards arrDst iDst iSrc n
                  EQ -> return ()
                  GT | (iDst - iSrc) * 2 < n
                        -> moveForwardsLargeOverlap arrDst iDst iSrc n
                     | otherwise
                        -> moveForwardsSmallOverlap arrDst iDst iSrc n
          | otherwise -> G.basicUnsafeCopy dst src

  {-# INLINE basicClear #-}
  basicClear v = G.set v uninitialised

{-# INLINE moveBackwards #-}
moveBackwards :: PrimMonad m => MutableArray (PrimState m) a -> Int -> Int -> Int -> m ()
moveBackwards !arr !dstOff !srcOff !len =
  INTERNAL_CHECK(check) "moveBackwards" "not a backwards move" (dstOff < srcOff)
  $ loopM len $ \ i -> readArray arr (srcOff + i) >>= writeArray arr (dstOff + i)

{-# INLINE moveForwardsSmallOverlap #-}
-- Performs a move when dstOff > srcOff, optimized for when the overlap of the intervals is small.
moveForwardsSmallOverlap :: PrimMonad m => MutableArray (PrimState m) a -> Int -> Int -> Int -> m ()
moveForwardsSmallOverlap !arr !dstOff !srcOff !len =
  INTERNAL_CHECK(check) "moveForwardsSmallOverlap" "not a forward move" (dstOff > srcOff)
  $ do
      tmp <- newArray overlap uninitialised
      loopM overlap $ \ i -> readArray arr (dstOff + i) >>= writeArray tmp i
      loopM nonOverlap $ \ i -> readArray arr (srcOff + i) >>= writeArray arr (dstOff + i)
      loopM overlap $ \ i -> readArray tmp i >>= writeArray arr (dstOff + nonOverlap + i)
  where nonOverlap = dstOff - srcOff; overlap = len - nonOverlap

-- Performs a move when dstOff > srcOff, optimized for when the overlap of the intervals is large.
moveForwardsLargeOverlap :: PrimMonad m => MutableArray (PrimState m) a -> Int -> Int -> Int -> m ()
moveForwardsLargeOverlap !arr !dstOff !srcOff !len =
  INTERNAL_CHECK(check) "moveForwardsLargeOverlap" "not a forward move" (dstOff > srcOff)
  $ do
      queue <- newArray nonOverlap uninitialised
      loopM nonOverlap $ \ i -> readArray arr (srcOff + i) >>= writeArray queue i
      let mov !i !qTop = when (i < dstOff + len) $ do
            x <- readArray arr i
            y <- readArray queue qTop
            writeArray arr i y
            writeArray queue qTop x
            mov (i+1) (if qTop + 1 >= nonOverlap then 0 else qTop + 1)
      mov dstOff 0
  where nonOverlap = dstOff - srcOff

{-# INLINE loopM #-}
loopM :: Monad m => Int -> (Int -> m a) -> m ()
loopM !n k = let
  go i = when (i < n) (k i >> go (i+1))
  in go 0

uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised element"

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: Int -> Int -> MVector s a -> MVector s a
{-# INLINE slice #-}
slice = G.slice

take :: Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

drop :: Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

{-# INLINE splitAt #-}
splitAt :: Int -> MVector s a -> (MVector s a, MVector s a)
splitAt = G.splitAt

init :: MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

tail :: MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: PrimMonad m => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The memory is not initialized.
unsafeNew :: PrimMonad m => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: PrimMonad m => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: PrimMonad m => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: PrimMonad m => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: PrimMonad m
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: PrimMonad m
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: PrimMonad m => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Swap the elements at the given positions.
swap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: PrimMonad m => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: PrimMonad m => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: PrimMonad m => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: PrimMonad m
                 => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: PrimMonad m
                 => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

