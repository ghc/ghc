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

  set, copy, move, unsafeCopy, unsafeMove,

  -- ** Arrays
  fromMutableArray, toMutableArray
) where

import           Control.Monad (when, liftM)
import qualified Data.Vector.Generic.Mutable as G
import           Data.Primitive.Array
import           Control.Monad.Primitive

import Prelude hiding ( length, null, replicate, reverse, read,
                        take, drop, splitAt, init, tail, foldr, foldl, mapM_ )

import Data.Typeable ( Typeable )

#include "vector.h"



-- | Mutable boxed vectors keyed on the monad they live in ('IO' or @'ST' s@).
data MVector s a = MVector {-# UNPACK #-} !Int                -- ^ Offset in underlying array
                           {-# UNPACK #-} !Int                -- ^ Size of slice
                           {-# UNPACK #-} !(MutableArray s a) -- ^ Underlying array
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
uninitialised = error "Data.Vector.Mutable: uninitialised element. If you are trying to compact a vector, use the 'Data.Vector.force' function to remove uninitialised elements from the underlying array."

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

-- | Yield a part of the mutable vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Int  -- ^ @i@ starting index
      -> Int  -- ^ @n@ length
      -> MVector s a
      -> MVector s a
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

-- | Create a mutable vector of the given length. The vector elements
--   are set to bottom so accessing them will cause an exception.
--
-- @since 0.5
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

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with the results of applying the function to each index.
--
-- @since 0.12.3.0
generate :: (PrimMonad m) => Int -> (Int -> a) -> m (MVector (PrimState m) a)
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Create a mutable vector of the given length (0 if the length is
-- negative) and fill it with the results of applying the monadic function to each
-- index. Iteration starts at index 0.
--
-- @since 0.12.3.0
generateM :: (PrimMonad m) => Int -> (Int -> m a) -> m (MVector (PrimState m) a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Create a copy of a mutable vector.
clone :: PrimMonad m => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a boxed vector by the given number of elements. The number must be
-- non-negative. Same semantics as in `G.grow` for generic vector. It differs
-- from @grow@ functions for unpacked vectors, however, in that only pointers to
-- values are copied over, therefore values themselves will be shared between
-- two vectors. This is an important distinction to know about during memory
-- usage analysis and in case when values themselves are of a mutable type, eg.
-- `Data.IORef.IORef` or another mutable vector.
--
-- ====__Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> mv <- V.thaw $ V.fromList ([10, 20, 30] :: [Integer])
-- >>> mv' <- MV.grow mv 2
--
-- The two extra elements at the end of the newly allocated vector will be
-- uninitialized and will result in an error if evaluated, so me must overwrite
-- them with new values first:
--
-- >>> MV.write mv' 3 999
-- >>> MV.write mv' 4 777
-- >>> V.unsafeFreeze mv'
-- [10,20,30,999,777]
--
-- It is important to note that the source mutable vector is not affected when
-- the newly allocated one is mutated.
--
-- >>> MV.write mv' 2 888
-- >>> V.unsafeFreeze mv'
-- [10,20,888,999,777]
-- >>> V.unsafeFreeze mv
-- [10,20,30]
--
-- @since 0.5
grow :: PrimMonad m
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be non-negative but
-- this is not checked. Same semantics as in `G.unsafeGrow` for generic vector.
--
-- @since 0.5
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

-- | Modify the element at the given position using a monadic function.
--
-- @since 0.12.3.0
modifyM :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE modifyM #-}
modifyM = G.modifyM

-- | Swap the elements at the given positions.
swap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap

-- | Replace the element at the given position and return the old element.
exchange :: (PrimMonad m) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE exchange #-}
exchange = G.exchange

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

-- | Modify the element at the given position using a monadic
-- function. No bounds checks are performed.
--
-- @since 0.12.3.0
unsafeModifyM :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m a) -> Int -> m ()
{-# INLINE unsafeModifyM #-}
unsafeModifyM = G.unsafeModifyM

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap :: PrimMonad m => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- | Replace the element at the given position and return the old element. No
-- bounds checks are performed.
unsafeExchange :: (PrimMonad m) => MVector (PrimState m) a -> Int -> a -> m a
{-# INLINE unsafeExchange #-}
unsafeExchange = G.unsafeExchange

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: PrimMonad m => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                    -> MVector (PrimState m) a   -- ^ source
                    -> m ()
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
move :: PrimMonad m => MVector (PrimState m) a   -- ^ target
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
unsafeMove :: PrimMonad m => MVector (PrimState m) a   -- ^ target
                          -> MVector (PrimState m) a   -- ^ source
                          -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

-- | Compute the next (lexicographically) permutation of given vector in-place.
--   Returns False when input is the last permutation
nextPermutation :: (PrimMonad m,Ord e) => MVector (PrimState m) e -> m Bool
{-# INLINE nextPermutation #-}
nextPermutation = G.nextPermutation


-- Folds
-- -----

-- | /O(n)/ Apply the monadic action to every element of the vector, discarding the results.
--
-- @since 0.12.3.0
mapM_ :: (PrimMonad m) => (a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of the vector and its index, discarding the results.
--
-- @since 0.12.3.0
imapM_ :: (PrimMonad m) => (Int -> a -> m b) -> MVector (PrimState m) a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to every element of the vector,
-- discarding the results. It's same as the @flip mapM_@.
--
-- @since 0.12.3.0
forM_ :: (PrimMonad m) => MVector (PrimState m) a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to every element of the vector
-- and its index, discarding the results. It's same as the @flip imapM_@.
--
-- @since 0.12.3.0
iforM_ :: (PrimMonad m) => MVector (PrimState m) a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- | /O(n)/ Pure left fold.
--
-- @since 0.12.3.0
foldl :: (PrimMonad m) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Pure left fold with strict accumulator.
--
-- @since 0.12.3.0
foldl' :: (PrimMonad m) => (b -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Pure left fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl :: (PrimMonad m) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Pure left fold with strict accumulator (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldl' :: (PrimMonad m) => (b -> Int -> a -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Pure right fold.
--
-- @since 0.12.3.0
foldr :: (PrimMonad m) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Pure right fold with strict accumulator.
--
-- @since 0.12.3.0
foldr' :: (PrimMonad m) => (a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Pure right fold (function applied to each element and its index).
--
-- @since 0.12.3.0
ifoldr :: (PrimMonad m) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Pure right fold with strict accumulator (function applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldr' :: (PrimMonad m) => (Int -> a -> b -> b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Monadic fold.
--
-- @since 0.12.3.0
foldM :: (PrimMonad m) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold with strict accumulator.
--
-- @since 0.12.3.0
foldM' :: (PrimMonad m) => (b -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM :: (PrimMonad m) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldM' :: (PrimMonad m) => (b -> Int -> a -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic right fold.
--
-- @since 0.12.3.0
foldrM :: (PrimMonad m) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM #-}
foldrM = G.foldrM

-- | /O(n)/ Monadic right fold with strict accumulator.
--
-- @since 0.12.3.0
foldrM' :: (PrimMonad m) => (a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE foldrM' #-}
foldrM' = G.foldrM'

-- | /O(n)/ Monadic right fold (action applied to each element and its index).
--
-- @since 0.12.3.0
ifoldrM :: (PrimMonad m) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM #-}
ifoldrM = G.ifoldrM

-- | /O(n)/ Monadic right fold with strict accumulator (action applied
-- to each element and its index).
--
-- @since 0.12.3.0
ifoldrM' :: (PrimMonad m) => (Int -> a -> b -> m b) -> b -> MVector (PrimState m) a -> m b
{-# INLINE ifoldrM' #-}
ifoldrM' = G.ifoldrM'

-- Conversions - Arrays
-- -----------------------------

-- | /O(n)/ Make a copy of a mutable array to a new mutable vector.
--
-- @since 0.12.2.0
fromMutableArray :: PrimMonad m => MutableArray (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE fromMutableArray #-}
fromMutableArray marr =
  let size = sizeofMutableArray marr
   in MVector 0 size `liftM` cloneMutableArray marr 0 size

-- | /O(n)/ Make a copy of a mutable vector into a new mutable array.
--
-- @since 0.12.2.0
toMutableArray :: PrimMonad m => MVector (PrimState m) a -> m (MutableArray (PrimState m) a)
{-# INLINE toMutableArray #-}
toMutableArray (MVector offset size marr) = cloneMutableArray marr offset size
