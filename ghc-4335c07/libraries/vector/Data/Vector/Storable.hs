{-# LANGUAGE CPP, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, Rank2Types, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Storable
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- 'Storable'-based vectors.
--

module Data.Vector.Storable (
  -- * Storable vectors
  Vector, MVector(..), Storable,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, create,

  -- ** Unfolding
  unfoldr, unfoldrN,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update_,
  unsafeUpd, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate_,
  unsafeAccum, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, mapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,

  -- ** Monadic zipping
  zipWithM, zipWithM_,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, foldM', fold1M, fold1M',
  foldM_, foldM'_, fold1M_, fold1M'_,

  -- * Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Other vector types
  G.convert, unsafeCast,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy,

  -- * Raw pointers
  unsafeFromForeignPtr, unsafeFromForeignPtr0,
  unsafeToForeignPtr,   unsafeToForeignPtr0,
  unsafeWith
) where

import qualified Data.Vector.Generic          as G
import           Data.Vector.Storable.Mutable ( MVector(..) )
import Data.Vector.Storable.Internal
import qualified Data.Vector.Fusion.Bundle as Bundle

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array ( advancePtr, copyArray )

import Control.DeepSeq ( NFData(rnf) )

import Control.Monad.ST ( ST )
import Control.Monad.Primitive

import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, minimum, maximum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )

import Data.Typeable ( Typeable )
import Data.Data     ( Data(..) )
import Text.Read     ( Read(..), readListPrecDefault )

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid   ( Monoid(..) )
#endif

#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as Exts
#endif

-- Data.Vector.Internal.Check is unused
#define NOT_VECTOR_MODULE
#include "vector.h"

-- | 'Storable'-based vectors
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)
        deriving ( Typeable )

instance NFData (Vector a) where
  rnf (Vector _ _) = ()

instance (Show a, Storable a) => Show (Vector a) where
  showsPrec = G.showsPrec

instance (Read a, Storable a) => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance (Data a, Storable a) => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = G.mkType "Data.Vector.Storable.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable Vector = MVector

instance Storable a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector n fp) = return $ Vector n fp

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector n fp) = return $ MVector n fp

  {-# INLINE basicLength #-}
  basicLength (Vector n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (Vector _ fp) = Vector n (updPtr (`advancePtr` i) fp)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector _ fp) i = return
                                    . unsafeInlineIO
                                    $ withForeignPtr fp $ \p ->
                                      peekElemOff p i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector n fp) (Vector _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE elemseq #-}
  elemseq _ = seq

-- See http://trac.haskell.org/vector/ticket/12
instance (Storable a, Eq a) => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (G.stream xs) (G.stream ys))

-- See http://trac.haskell.org/vector/ticket/12
instance (Storable a, Ord a) => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Storable a => Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = (++)

  {-# INLINE mconcat #-}
  mconcat = concat

#if __GLASGOW_HASKELL__ >= 708

instance Storable a => Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = fromList
  fromListN = fromListN
  toList = toList

#endif

-- Length
-- ------

-- | /O(1)/ Yield the length of the vector.
length :: Storable a => Vector a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector if empty
null :: Storable a => Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing
(!) :: Storable a => Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing
(!?) :: Storable a => Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element
head :: Storable a => Vector a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element
last :: Storable a => Vector a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: Storable a => Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: Storable a => Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: Storable a => Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- Monadic indexing
-- ----------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- elements) is evaluated eagerly.
--
indexM :: (Storable a, Monad m) => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Storable a, Monad m) => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Storable a, Monad m) => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Storable a
      => Int   -- ^ @i@ starting index
      -> Int   -- ^ @n@ length
      -> Vector a
      -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Storable a => Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Storable a => Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: Storable a => Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
{-# INLINE splitAt #-}
splitAt :: Storable a => Int -> Vector a -> (Vector a, Vector a)
splitAt = G.splitAt

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: Storable a => Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: Storable a => Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: Storable a => Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: Storable a => Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: Storable a => Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: Storable a => Vector a
{-# INLINE empty #-}
empty = G.empty

-- | /O(1)/ Vector with exactly one element
singleton :: Storable a => a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: Storable a => Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: Storable a => Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
iterateN :: Storable a => Int -> (a -> a) -> a -> Vector a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: Storable a => (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ by repeatedly applying the
-- generator function to the a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Storable a => Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in f <a,b,c>
--
constructN :: Storable a => Int -> (Vector a -> a) -> Vector a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in f <c,b,a>
--
constructrN :: Storable a => Int -> (Vector a -> a) -> Vector a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Storable a, Num a) => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: (Storable a, Num a) => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Storable a, Enum a) => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Storable a, Enum a) => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: Storable a => a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element
snoc :: Storable a => Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: Storable a => Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list
concat :: Storable a => [Vector a] -> Vector a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Storable a) => Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, Storable a) => Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: Storable a => (forall s. ST s (MVector s a)) -> Vector a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p

-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: Storable a => Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: Storable a => Vector a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
update_ :: Storable a
        => Vector a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector a   -- ^ value vector (of length @n2@)
        -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

-- | Same as ('//') but without bounds checking.
unsafeUpd :: Storable a => Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: Storable a => Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: Storable a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
{-# INLINE accum #-}
accum = G.accum

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
accumulate_ :: (Storable a, Storable b)
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: Storable a => (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (Storable a, Storable b) =>
               (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: Storable a => Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Storable a => Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: Storable a => Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> write v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: Storable a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify p = G.modify p

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Storable a, Storable b) => (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
concatMap :: (Storable a, Storable b) => (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, Storable a, Storable b) => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, Storable a) => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
forM :: (Monad m, Storable a, Storable b) => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Storable a) => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (Storable a, Storable b, Storable c)
        => (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (Storable a, Storable b, Storable c, Storable d)
         => (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
         => (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
             Storable f, Storable g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Storable a, Storable b, Storable c)
         => (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Storable a, Storable b, Storable c, Storable d)
          => (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
          => (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f)
          => (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
              Storable f, Storable g)
          => (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, Storable a, Storable b, Storable c)
         => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, Storable a, Storable b)
          => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: Storable a => (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Storable a) => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate
-- without copying.
takeWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: Storable a => (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: Storable a => (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (Storable a, Eq a) => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Storable a => (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Storable a => (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: Storable a => (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Storable a, Eq a) => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (Storable a, Eq a) => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Storable b => (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold
foldr :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: Storable a => (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: Storable a => (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: Storable b => (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: Storable a => (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
any :: Storable a => (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements
sum :: (Storable a, Num a) => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the produce of the elements
product :: (Storable a, Num a) => Vector a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
maximum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
minimum :: (Storable a, Ord a) => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: Storable a => (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Storable a, Ord a) => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: Storable a => (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Storable b) => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Storable a) => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Prefix sums (scans)
-- -------------------

-- | /O(n)/ Prescan
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
scanl :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (Storable a, Storable b) => (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left scan
postscanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (Storable a, Storable b) => (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: Storable a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: Storable a => Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector
fromList :: Storable a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Storable a => Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Unsafe casts
-- --------------------------

-- | /O(1)/ Unsafely cast a vector from one element type to another.
-- The operation just changes the type of the underlying pointer and does not
-- modify the elements.
--
-- The resulting vector contains as many elements as can fit into the
-- underlying memory block.
--
unsafeCast :: forall a b. (Storable a, Storable b) => Vector a -> Vector b
{-# INLINE unsafeCast #-}
unsafeCast (Vector n fp)
  = Vector ((n * sizeOf (undefined :: a)) `div` sizeOf (undefined :: b))
           (castForeignPtr fp)


-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
        :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw
        :: (Storable a, PrimMonad m) => Vector a -> m (MVector (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (Storable a, PrimMonad m) => Vector a -> m (MVector (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> m (Vector a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: (Storable a, PrimMonad m) => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- Conversions - Raw pointers
-- --------------------------

-- | /O(1)/ Create a vector from a 'ForeignPtr' with an offset and a length.
--
-- The data may not be modified through the 'ForeignPtr' afterwards.
--
-- If your offset is 0 it is more efficient to use 'unsafeFromForeignPtr0'.
unsafeFromForeignPtr :: Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> Vector a
{-# INLINE_FUSED unsafeFromForeignPtr #-}
unsafeFromForeignPtr fp i n = unsafeFromForeignPtr0 fp' n
    where
      fp' = updPtr (`advancePtr` i) fp

{-# RULES
"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n " forall fp n.
  unsafeFromForeignPtr fp 0 n = unsafeFromForeignPtr0 fp n   #-}


-- | /O(1)/ Create a vector from a 'ForeignPtr' and a length.
--
-- It is assumed the pointer points directly to the data (no offset).
-- Use `unsafeFromForeignPtr` if you need to specify an offset.
--
-- The data may not be modified through the 'ForeignPtr' afterwards.
unsafeFromForeignPtr0 :: Storable a
                      => ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> Vector a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = Vector n fp

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with the offset to the
-- data and its length. The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr :: Storable a => Vector a -> (ForeignPtr a, Int, Int)
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (Vector n fp) = (fp, 0, n)

-- | /O(1)/ Yield the underlying 'ForeignPtr' together with its length.
--
-- You can assume the pointer points directly to the data (no offset).
--
-- The data may not be modified through the 'ForeignPtr'.
unsafeToForeignPtr0 :: Storable a => Vector a -> (ForeignPtr a, Int)
{-# INLINE unsafeToForeignPtr0 #-}
unsafeToForeignPtr0 (Vector n fp) = (fp, n)

-- | Pass a pointer to the vector's data to the IO action. The data may not be
-- modified through the 'Ptr.
unsafeWith :: Storable a => Vector a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (Vector _ fp) = withForeignPtr fp


