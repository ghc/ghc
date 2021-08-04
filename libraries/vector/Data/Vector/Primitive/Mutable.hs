{-# LANGUAGE CPP, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Primitive.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable primitive vectors.
--

module Data.Vector.Primitive.Mutable (
  -- * Mutable vectors of primitive types
  MVector(..), IOVector, STVector, Prim,

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
  new, unsafeNew, replicate, replicateM, generate, generateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, modify, modifyM, swap, exchange,
  unsafeRead, unsafeWrite, unsafeModify, unsafeModifyM, unsafeSwap, unsafeExchange,

  -- * Folds
  mapM_, imapM_, forM_, iforM_,
  foldl, foldl', foldM, foldM',
  foldr, foldr', foldrM, foldrM',
  ifoldl, ifoldl', ifoldM, ifoldM',
  ifoldr, ifoldr', ifoldrM, ifoldrM',

  -- * Modifying vectors
  nextPermutation,

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove
) where

import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.ByteArray
import           Data.Primitive ( Prim, sizeOf )
import           Data.Word ( Word8 )
import           Control.Monad.Primitive
import           Control.Monad ( liftM )

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail, foldr, foldl, mapM_ )

import Data.Typeable ( Typeable )

-- Data.Vector.Internal.Check is unnecessary
#define NOT_VECTOR_MODULE
#include "vector.h"

-- | Mutable vectors of primitive types.
data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(MutableByteArray s) -- ^ offset, length, underlying mutable byte array
        deriving ( Typeable )

type IOVector = MVector RealWorld
type STVector s = MVector s

instance NFData (MVector s a) where
  rnf (MVector _ _ _) = ()

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 (MVector s) where
  liftRnf _ (MVector _ _ _) = ()
#endif

instance Prim a => G.MVector MVector a where
  basicLength (MVector _ n _) = n
  basicUnsafeSlice j m (MVector i _ arr)
    = MVector (i+j) m arr

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector i m arr1) (MVector j n arr2)
    = sameMutableByteArray arr1 arr2
      && (between i j (j+n) || between j i (i+m))
    where
      between x y z = x >= y && x < z

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Primitive.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Primitive.basicUnsafeNew: length to large: " ++ show n
    | otherwise = MVector 0 n `liftM` newByteArray (n * size)
    where
      size = sizeOf (undefined :: a)
      mx = maxBound `div` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize (MVector off n v) =
      setByteArray v (off * size) (n * size) (0 :: Word8)
    where
      size = sizeOf (undefined :: a)


  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector i _ arr) j = readByteArray arr (i+j)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector i _ arr) j x = writeByteArray arr (i+j) x

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector i n dst) (MVector j _ src)
    = moveByteArray dst (i*sz) src (j*sz) (n * sz)
    where
      sz = sizeOf (undefined :: a)

  {-# INLINE basicSet #-}
  basicSet (MVector i n arr) x = setByteArray arr i n x

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: Prim a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: Prim a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Prim a
      => Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
{-# INLINE slice #-}
slice = G.slice

take :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

drop :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

splitAt :: Prim a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

init :: Prim a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

tail :: Prim a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: Prim a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: Prim a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: Prim a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: Prim a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- | Check whether two vectors overlap.
overlaps :: Prim a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, Prim a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The vector content
--   is uninitialized, which means it is filled with whatever underlying memory
--   buffer happens to contain.
--
-- @since 0.5
unsafeNew :: (PrimMonad m, Prim a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, Prim a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, Prim a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
--
-- @since 0.12.3.0
generate :: (PrimMonad m, Prim a) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m, Prim a) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, Prim a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a primitive vector by the given number of elements. The number must be
-- non-negative. Same semantics as in `G.grow` for generic vector.
--
-- ====__Examples__
--
-- >>> import qualified Data.Vector.Primitive as VP
-- >>> import qualified Data.Vector.Primitive.Mutable as MVP
-- >>> mv <- VP.thaw $ VP.fromList ([10, 20, 30] :: [Int])
-- >>> mv' <- MVP.grow mv 2
--
-- Extra memory at the end of the newly allocated vector is initialized to 0
-- bytes, which for `Prim` instance will usually correspond to some default
-- value for a particular type, eg. @0@ for @Int@, @\NUL@ for @Char@,
-- etc. However, if `unsafeGrow` was used instead this would not have been
-- guaranteed and some garbage would be there instead:
--
-- >>> VP.unsafeFreeze mv'
-- [10,20,30,0,0]
--
-- Having the extra space we can write new values in there:
--
-- >>> MVP.write mv' 3 999
-- >>> VP.unsafeFreeze mv'
-- [10,20,30,999,0]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MVP.write mv' 2 888
-- >>> VP.unsafeFreeze mv'
-- [10,20,888,999,0]
-- >>> VP.unsafeFreeze mv
-- [10,20,30]
--
-- @since 0.5
grow :: (PrimMonad m, Prim a)
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative but
-- this is not checked. Same semantics as in `G.unsafeGrow` for generic vector.
--
-- @since 0.5
unsafeGrow :: (PrimMonad m, Prim a)
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Modify the element at the given position.
modify :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE modify #-}
modify = G.modify

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
exchange :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite
    :: (PrimMonad m, Prim a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Modify the element at the given position. No bounds checks are performed.
unsafeModify :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
{-# INLINE unsafeModify #-}
unsafeModify = G.unsafeModify

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, Prim a)
           => MVector (PrimState m) a   -- ^ target
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
move :: (PrimMonad m, Prim a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, Prim a)
                          => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permutation
nextPermutation :: (PrimMonad m,Ord e,Prim e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation


-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m, Prim a) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m, Prim a) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's same as the @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's same as the @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m, Prim a) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m, Prim a) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m, Prim a) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m, Prim a) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator (function applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m, Prim a) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m, Prim a) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m, Prim a) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m, Prim a) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator (action applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m, Prim a) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'
