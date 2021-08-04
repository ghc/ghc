{-# LANGUAGE CPP, Rank2Types, MultiParamTypeClasses, FlexibleContexts,
             TypeFamilies, ScopedTypeVariables, BangPatterns #-}
-- |
-- Module      : Data.Vector.Generic
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Generic interface to pure vectors.
--

module Data.Vector.Generic (
  -- * Immutable vectors
  Vector(..), Mutable,

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
  slice, init, tail, take, drop, splitAt, uncons, unsnoc,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, iterateNM, create, createT,

  -- ** Unfolding
  unfoldr, unfoldrN, unfoldrExactN,
  unfoldrM, unfoldrNM, unfoldrExactNM,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat, concatNE,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, imapM, mapM_, imapM_, forM, forM_,
  iforM, iforM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM, uniq,
  mapMaybe, imapMaybe,
  mapMaybeM, imapMaybeM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, partitionWith, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndexR, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, ifoldM, foldM', ifoldM',
  fold1M, fold1M', foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- ** Monadic sequencing
  sequence, sequence_,

  -- * Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  iscanl, iscanl',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',
  iscanr, iscanr',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Different vector types
  convert,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy,

  -- * Fusion support

  -- ** Conversion to/from Bundles
  stream, unstream, unstreamM, streamR, unstreamR,

  -- ** Recycling support
  new, clone,

  -- * Utilities

  -- ** Comparisons
  eq, cmp,
  eqBy, cmpBy,

  -- ** Show and Read
  showsPrec, readPrec,
  liftShowsPrec, liftReadsPrec,

  -- ** @Data@ and @Typeable@
  gfoldl, gunfold, dataCast, mkVecType, mkVecConstr, mkType
) where

import           Data.Vector.Generic.Base

import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )

import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Bundle ( Bundle, MBundle, lift, inplace )
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           Data.Vector.Fusion.Bundle.Size
import           Data.Vector.Fusion.Util

import Control.Monad.ST ( ST, runST )
import Control.Monad.Primitive
import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concat, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
#if __GLASGOW_HASKELL__ >= 706
                        foldMap,
#endif
                        all, any, and, or, sum, product, maximum, minimum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_,
                        showsPrec )

import qualified Text.Read as Read
import qualified Data.List.NonEmpty as NonEmpty

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

#if __GLASGOW_HASKELL__ >= 707
import Data.Typeable ( Typeable, gcast1 )
#else
import Data.Typeable ( Typeable1, gcast1 )
#endif

#include "vector.h"

import Data.Data ( Data, DataType, Constr, Fixity(Prefix),
                   mkDataType, mkConstr, constrIndex,
#if MIN_VERSION_base(4,2,0)
                   mkNoRepType )
#else
                   mkNorepType )
#endif
import qualified Data.Traversable as T (Traversable(mapM))

#if !MIN_VERSION_base(4,2,0)
mkNoRepType :: String -> DataType
mkNoRepType = mkNorepType
#endif

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector
length :: Vector v a => v a -> Int
{-# INLINE length #-}
length = Bundle.length . stream

-- | /O(1)/ Test whether a vector is empty
null :: Vector v a => v a -> Bool
{-# INLINE null #-}
null = Bundle.null . stream

-- Indexing
-- --------

infixl 9 !
-- | O(1) Indexing
(!) :: Vector v a => v a -> Int -> a
{-# INLINE_FUSED (!) #-}
(!) v i = BOUNDS_CHECK(checkIndex) "(!)" i (length v)
        $ unId (basicUnsafeIndexM v i)

infixl 9 !?
-- | O(1) Safe indexing
(!?) :: Vector v a => v a -> Int -> Maybe a
{-# INLINE_FUSED (!?) #-}
v !? i | i < 0 || i >= length v = Nothing
       | otherwise              = Just $ unsafeIndex v i

-- | /O(1)/ First element
head :: Vector v a => v a -> a
{-# INLINE_FUSED head #-}
head v = v ! 0

-- | /O(1)/ Last element
last :: Vector v a => v a -> a
{-# INLINE_FUSED last #-}
last v = v ! (length v - 1)

-- | /O(1)/ Unsafe indexing without bounds checking
unsafeIndex :: Vector v a => v a -> Int -> a
{-# INLINE_FUSED unsafeIndex #-}
unsafeIndex v i = UNSAFE_CHECK(checkIndex) "unsafeIndex" i (length v)
                $ unId (basicUnsafeIndexM v i)

-- | /O(1)/ First element without checking if the vector is empty
unsafeHead :: Vector v a => v a -> a
{-# INLINE_FUSED unsafeHead #-}
unsafeHead v = unsafeIndex v 0

-- | /O(1)/ Last element without checking if the vector is empty
unsafeLast :: Vector v a => v a -> a
{-# INLINE_FUSED unsafeLast #-}
unsafeLast v = unsafeIndex v (length v - 1)

{-# RULES

"(!)/unstream [Vector]" forall i s.
  new (New.unstream s) ! i = s Bundle.!! i

"(!?)/unstream [Vector]" forall i s.
  new (New.unstream s) !? i = s Bundle.!? i

"head/unstream [Vector]" forall s.
  head (new (New.unstream s)) = Bundle.head s

"last/unstream [Vector]" forall s.
  last (new (New.unstream s)) = Bundle.last s

"unsafeIndex/unstream [Vector]" forall i s.
  unsafeIndex (new (New.unstream s)) i = s Bundle.!! i

"unsafeHead/unstream [Vector]" forall s.
  unsafeHead (new (New.unstream s)) = Bundle.head s

"unsafeLast/unstream [Vector]" forall s.
  unsafeLast (new (New.unstream s)) = Bundle.last s  #-}



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
indexM :: (Vector v a, Monad m) => v a -> Int -> m a
{-# INLINE_FUSED indexM #-}
indexM v i = BOUNDS_CHECK(checkIndex) "indexM" i (length v)
           $ basicUnsafeIndexM v i

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_FUSED headM #-}
headM v = indexM v 0

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_FUSED lastM #-}
lastM v = indexM v (length v - 1)

-- | /O(1)/ Indexing in a monad without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: (Vector v a, Monad m) => v a -> Int -> m a
{-# INLINE_FUSED unsafeIndexM #-}
unsafeIndexM v i = UNSAFE_CHECK(checkIndex) "unsafeIndexM" i (length v)
                 $ basicUnsafeIndexM v i

-- | /O(1)/ First element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_FUSED unsafeHeadM #-}
unsafeHeadM v = unsafeIndexM v 0

-- | /O(1)/ Last element in a monad without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: (Vector v a, Monad m) => v a -> m a
{-# INLINE_FUSED unsafeLastM #-}
unsafeLastM v = unsafeIndexM v (length v - 1)

{-# RULES

"indexM/unstream [Vector]" forall s i.
  indexM (new (New.unstream s)) i = lift s MBundle.!! i

"headM/unstream [Vector]" forall s.
  headM (new (New.unstream s)) = MBundle.head (lift s)

"lastM/unstream [Vector]" forall s.
  lastM (new (New.unstream s)) = MBundle.last (lift s)

"unsafeIndexM/unstream [Vector]" forall s i.
  unsafeIndexM (new (New.unstream s)) i = lift s MBundle.!! i

"unsafeHeadM/unstream [Vector]" forall s.
  unsafeHeadM (new (New.unstream s)) = MBundle.head (lift s)

"unsafeLastM/unstream [Vector]" forall s.
  unsafeLastM (new (New.unstream s)) = MBundle.last (lift s)   #-}



-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Vector v a => Int   -- ^ @i@ starting index
                    -> Int   -- ^ @n@ length
                    -> v a
                    -> v a
{-# INLINE_FUSED slice #-}
slice i n v = BOUNDS_CHECK(checkSlice) "slice" i n (length v)
            $ basicUnsafeSlice i n v

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Vector v a => v a -> v a
{-# INLINE_FUSED init #-}
init v = slice 0 (length v - 1) v

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Vector v a => v a -> v a
{-# INLINE_FUSED tail #-}
tail v = slice 1 (length v - 1) v

-- | /O(1)/ Yield the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case it is returned unchanged.
take :: Vector v a => Int -> v a -> v a
{-# INLINE_FUSED take #-}
take n v = unsafeSlice 0 (delay_inline min n' (length v)) v
  where n' = max n 0

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements in which case an empty vector is returned.
drop :: Vector v a => Int -> v a -> v a
{-# INLINE_FUSED drop #-}
drop n v = unsafeSlice (delay_inline min n' len)
                       (delay_inline max 0 (len - n')) v
  where n' = max n 0
        len = length v

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@
-- but slightly more efficient.
--
-- @since 0.7.1
splitAt :: Vector v a => Int -> v a -> (v a, v a)
{-# INLINE_FUSED splitAt #-}
splitAt n v = ( unsafeSlice 0 m v
              , unsafeSlice m (delay_inline max 0 (len - n')) v
              )
    where
      m   = delay_inline min n' len
      n'  = max n 0
      len = length v

-- | /O(1)/ Yield the 'head' and 'tail' of the vector, or 'Nothing' if empty.
--
-- @since 0.12.2.0
uncons :: Vector v a => v a -> Maybe (a, v a)
{-# INLINE_FUSED uncons #-}
uncons xs = flip (,) (unsafeTail xs) `fmap` (xs !? 0)

-- | /O(1)/ Yield the 'last' and 'init' of the vector, or 'Nothing' if empty.
--
-- @since 0.12.2.0
unsnoc :: Vector v a => v a -> Maybe (v a, a)
{-# INLINE_FUSED unsnoc #-}
unsnoc xs = (,) (unsafeInit xs) `fmap` (xs !? (length xs - 1))

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements but this is not checked.
unsafeSlice :: Vector v a => Int   -- ^ @i@ starting index
                          -> Int   -- ^ @n@ length
                          -> v a
                          -> v a
{-# INLINE_FUSED unsafeSlice #-}
unsafeSlice i n v = UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                  $ basicUnsafeSlice i n v

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty but this is not checked.
unsafeInit :: Vector v a => v a -> v a
{-# INLINE_FUSED unsafeInit #-}
unsafeInit v = unsafeSlice 0 (length v - 1) v

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty but this is not checked.
unsafeTail :: Vector v a => v a -> v a
{-# INLINE_FUSED unsafeTail #-}
unsafeTail v = unsafeSlice 1 (length v - 1) v

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements but this is not checked.
unsafeTake :: Vector v a => Int -> v a -> v a
{-# INLINE unsafeTake #-}
unsafeTake n v = unsafeSlice 0 n v

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements but this is not checked.
unsafeDrop :: Vector v a => Int -> v a -> v a
{-# INLINE unsafeDrop #-}
unsafeDrop n v = unsafeSlice n (length v - n) v


-- Turned off due to: https://github.com/haskell/vector/issues/257
-- "slice/new [Vector]" forall i n p.
--   slice i n (new p) = new (New.slice i n p)

{-# RULES

"init/new [Vector]" forall p.
  init (new p) = new (New.init p)

"tail/new [Vector]" forall p.
  tail (new p) = new (New.tail p)

"take/new [Vector]" forall n p.
  take n (new p) = new (New.take n p)

"drop/new [Vector]" forall n p.
  drop n (new p) = new (New.drop n p)

"unsafeSlice/new [Vector]" forall i n p.
  unsafeSlice i n (new p) = new (New.unsafeSlice i n p)

"unsafeInit/new [Vector]" forall p.
  unsafeInit (new p) = new (New.unsafeInit p)

"unsafeTail/new [Vector]" forall p.
  unsafeTail (new p) = new (New.unsafeTail p)   #-}



-- Initialisation
-- --------------

-- | /O(1)/ Empty vector
empty :: Vector v a => v a
{-# INLINE empty #-}
empty = unstream Bundle.empty

-- | /O(1)/ Vector with exactly one element
singleton :: forall v a. Vector v a => a -> v a
{-# INLINE singleton #-}
singleton x = elemseq (undefined :: v a) x
            $ unstream (Bundle.singleton x)

-- | /O(n)/ Vector of the given length with the same value in each position
replicate :: forall v a. Vector v a => Int -> a -> v a
{-# INLINE replicate #-}
replicate n x = elemseq (undefined :: v a) x
              $ unstream
              $ Bundle.replicate n x

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index
generate :: Vector v a => Int -> (Int -> a) -> v a
{-# INLINE generate #-}
generate n f = unstream (Bundle.generate n f)

-- | /O(n)/ Apply function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). Zeroth element will contain the initial value, that's why there
-- is one less function application than the number of elements in the produced vector.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- @since 0.7.1
iterateN :: Vector v a => Int -> (a -> a) -> a -> v a
{-# INLINE iterateN #-}
iterateN n f x = unstream (Bundle.iterateN n f x)

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: Vector v a => (b -> Maybe (a, b)) -> b -> v a
{-# INLINE unfoldr #-}
unfoldr f = unstream . Bundle.unfoldr f

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN  :: Vector v a => Int -> (b -> Maybe (a, b)) -> b -> v a
{-# INLINE unfoldrN #-}
unfoldrN n f = unstream . Bundle.unfoldrN n f

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
--
-- @since 0.12.2.0
unfoldrExactN  :: Vector v a => Int -> (b -> (a, b)) -> b -> v a
{-# INLINE unfoldrExactN #-}
unfoldrExactN n f = unstream . Bundle.unfoldrExactN n f

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrM :: (Monad m, Vector v a) => (b -> m (Maybe (a, b))) -> b -> m (v a)
{-# INLINE unfoldrM #-}
unfoldrM f = unstreamM . MBundle.unfoldrM f

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrNM :: (Monad m, Vector v a) => Int -> (b -> m (Maybe (a, b))) -> b -> m (v a)
{-# INLINE unfoldrNM #-}
unfoldrNM n f = unstreamM . MBundle.unfoldrNM n f

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly
-- applying the monadic generator function to a seed. The generator
-- function yields the next element and the new seed.
--
-- @since 0.12.2.0
unfoldrExactNM :: (Monad m, Vector v a) => Int -> (b -> m (a, b)) -> b -> m (v a)
{-# INLINE unfoldrExactNM #-}
unfoldrExactNM n f = unstreamM . MBundle.unfoldrExactNM n f

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
--
constructN :: forall v a. Vector v a => Int -> (v a -> a) -> v a
{-# INLINE constructN #-}
-- NOTE: We *CANNOT* wrap this in New and then fuse because the elements
-- might contain references to the immutable vector!
constructN !n f = runST (
  do
    v  <- M.new n
    v' <- unsafeFreeze v
    fill v' 0
  )
  where
    fill :: forall s. v a -> Int -> ST s (v a)
    fill !v i | i < n = let x = f (unsafeTake i v)
                        in
                        elemseq v x $
                        do
                          v'  <- unsafeThaw v
                          M.unsafeWrite v' i x
                          v'' <- unsafeFreeze v'
                          fill v'' (i+1)

    fill v _ = return v

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
--
constructrN :: forall v a. Vector v a => Int -> (v a -> a) -> v a
{-# INLINE constructrN #-}
-- NOTE: We *CANNOT* wrap this in New and then fuse because the elements
-- might contain references to the immutable vector!
constructrN !n f = runST (
  do
    v  <- n `seq` M.new n
    v' <- unsafeFreeze v
    fill v' 0
  )
  where
    fill :: forall s. v a -> Int -> ST s (v a)
    fill !v i | i < n = let x = f (unsafeSlice (n-i) i v)
                        in
                        elemseq v x $
                        do
                          v'  <- unsafeThaw v
                          M.unsafeWrite v' (n-i-1) x
                          v'' <- unsafeFreeze v'
                          fill v'' (i+1)

    fill v _ = return v


-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: (Vector v a, Num a) => a -> Int -> v a
{-# INLINE enumFromN #-}
enumFromN x n = enumFromStepN x 1 n

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 0.1 5 = <1,1.1,1.2,1.3,1.4>
enumFromStepN :: forall v a. (Vector v a, Num a) => a -> a -> Int -> v a
{-# INLINE enumFromStepN #-}
enumFromStepN x y n = elemseq (undefined :: v a) x
                    $ elemseq (undefined :: v a) y
                    $ unstream
                    $ Bundle.enumFromStepN  x y n

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromN' instead.
enumFromTo :: (Vector v a, Enum a) => a -> a -> v a
{-# INLINE enumFromTo #-}
enumFromTo x y = unstream (Bundle.enumFromTo x y)

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Vector v a, Enum a) => a -> a -> a -> v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo x y z = unstream (Bundle.enumFromThenTo x y z)

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element
cons :: forall v a. Vector v a => a -> v a -> v a
{-# INLINE cons #-}
cons x v = elemseq (undefined :: v a) x
         $ unstream
         $ Bundle.cons x
         $ stream v

-- | /O(n)/ Append an element
snoc :: forall v a. Vector v a => v a -> a -> v a
{-# INLINE snoc #-}
snoc v x = elemseq (undefined :: v a) x
         $ unstream
         $ Bundle.snoc (stream v) x

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors
(++) :: Vector v a => v a -> v a -> v a
{-# INLINE (++) #-}
v ++ w = unstream (stream v Bundle.++ stream w)

-- | /O(n)/ Concatenate all vectors in the list
concat :: Vector v a => [v a] -> v a
{-# INLINE concat #-}
concat = unstream . Bundle.fromVectors
{-
concat vs = unstream (Bundle.flatten mk step (Exact n) (Bundle.fromList vs))
  where
    n = List.foldl' (\k v -> k + length v) 0 vs

    {-# INLINE_INNER step #-}
    step (v,i,k)
      | i < k = case unsafeIndexM v i of
                  Box x -> Bundle.Yield x (v,i+1,k)
      | otherwise = Bundle.Done

    {-# INLINE mk #-}
    mk v = let k = length v
           in
           k `seq` (v,0,k)
-}

-- | /O(n)/ Concatenate all vectors in the non-empty list
concatNE :: Vector v a => NonEmpty.NonEmpty (v a) -> v a
concatNE = concat . NonEmpty.toList

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: (Monad m, Vector v a) => Int -> m a -> m (v a)
{-# INLINE replicateM #-}
replicateM n m = unstreamM (MBundle.replicateM n m)

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index
generateM :: (Monad m, Vector v a) => Int -> (Int -> m a) -> m (v a)
{-# INLINE generateM #-}
generateM n f = unstreamM (MBundle.generateM n f)

-- | /O(n)/ Apply monadic function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). Zeroth element will contain the initial value, that's why there
-- is one less function application than the number of elements in the produced vector.
--
-- For non-monadic version see `iterateN`
--
-- @since 0.12.0.0
iterateNM :: (Monad m, Vector v a) => Int -> (a -> m a) -> a -> m (v a)
{-# INLINE iterateNM #-}
iterateNM n f x = unstreamM (MBundle.iterateNM n f x)

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- 'M.new' 2; 'M.write' v 0 \'a\'; 'M.write' v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: Vector v a => (forall s. ST s (Mutable v s a)) -> v a
{-# INLINE create #-}
create p = new (New.create p)

-- | Execute the monadic action and freeze the resulting vectors.
createT
  :: (T.Traversable f, Vector v a)
  => (forall s. ST s (f (Mutable v s a))) -> f (v a)
{-# INLINE createT #-}
createT p = runST (p >>= T.mapM unsafeFreeze)

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
force :: Vector v a => v a -> v a
-- FIXME: we probably ought to inline this later as the rules still might fire
-- otherwise
{-# INLINE_FUSED force #-}
force v = new (clone v)

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: Vector v a => v a        -- ^ initial vector (of length @m@)
                   -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                   -> v a
{-# INLINE (//) #-}
v // us = update_stream v (Bundle.fromList us)

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
update :: (Vector v a, Vector v (Int, a))
        => v a        -- ^ initial vector (of length @m@)
        -> v (Int, a) -- ^ vector of index/value pairs (of length @n@)
        -> v a
{-# INLINE update #-}
update v w = update_stream v (stream w)

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
-- This function is useful for instances of 'Vector' that cannot store pairs.
-- Otherwise, 'update' is probably more convenient.
--
-- @
-- update_ xs is ys = 'update' xs ('zip' is ys)
-- @
update_ :: (Vector v a, Vector v Int)
        => v a   -- ^ initial vector (of length @m@)
        -> v Int -- ^ index vector (of length @n1@)
        -> v a   -- ^ value vector (of length @n2@)
        -> v a
{-# INLINE update_ #-}
update_ v is w = update_stream v (Bundle.zipWith (,) (stream is) (stream w))

update_stream :: Vector v a => v a -> Bundle u (Int,a) -> v a
{-# INLINE update_stream #-}
update_stream = modifyWithBundle M.update

-- | Same as ('//') but without bounds checking.
unsafeUpd :: Vector v a => v a -> [(Int, a)] -> v a
{-# INLINE unsafeUpd #-}
unsafeUpd v us = unsafeUpdate_stream v (Bundle.fromList us)

-- | Same as 'update' but without bounds checking.
unsafeUpdate :: (Vector v a, Vector v (Int, a)) => v a -> v (Int, a) -> v a
{-# INLINE unsafeUpdate #-}
unsafeUpdate v w = unsafeUpdate_stream v (stream w)

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: (Vector v a, Vector v Int) => v a -> v Int -> v a -> v a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ v is w
  = unsafeUpdate_stream v (Bundle.zipWith (,) (stream is) (stream w))

unsafeUpdate_stream :: Vector v a => v a -> Bundle u (Int,a) -> v a
{-# INLINE unsafeUpdate_stream #-}
unsafeUpdate_stream = modifyWithBundle M.unsafeUpdate

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accum (+) (V.fromList [1000.0,2000.0,3000.0]) [(2,4),(1,6),(0,3),(1,10)]
-- [1003.0,2016.0,3004.0]
accum :: Vector v a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> v a           -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> v a
{-# INLINE accum #-}
accum f v us = accum_stream f v (Bundle.fromList us)

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- element @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accumulate (+) (V.fromList [1000.0,2000.0,3000.0]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- [1003.0,2016.0,3004.0]
accumulate :: (Vector v a, Vector v (Int, b))
           => (a -> b -> a) -- ^ accumulating function @f@
           -> v a           -- ^ initial vector (of length @m@)
           -> v (Int,b)     -- ^ vector of index/value pairs (of length @n@)
           -> v a
{-# INLINE accumulate #-}
accumulate f v us = accum_stream f v (stream us)

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
-- This function is useful for instances of 'Vector' that cannot store pairs.
-- Otherwise, 'accumulate' is probably more convenient:
--
-- @
-- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- @
accumulate_ :: (Vector v a, Vector v Int, Vector v b)
                => (a -> b -> a) -- ^ accumulating function @f@
                -> v a           -- ^ initial vector (of length @m@)
                -> v Int         -- ^ index vector (of length @n1@)
                -> v b           -- ^ value vector (of length @n2@)
                -> v a
{-# INLINE accumulate_ #-}
accumulate_ f v is xs = accum_stream f v (Bundle.zipWith (,) (stream is)
                                                             (stream xs))


accum_stream :: Vector v a => (a -> b -> a) -> v a -> Bundle u (Int,b) -> v a
{-# INLINE accum_stream #-}
accum_stream f = modifyWithBundle (M.accum f)

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: Vector v a => (a -> b -> a) -> v a -> [(Int,b)] -> v a
{-# INLINE unsafeAccum #-}
unsafeAccum f v us = unsafeAccum_stream f v (Bundle.fromList us)

-- | Same as 'accumulate' but without bounds checking.
unsafeAccumulate :: (Vector v a, Vector v (Int, b))
                => (a -> b -> a) -> v a -> v (Int,b) -> v a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate f v us = unsafeAccum_stream f v (stream us)

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (Vector v a, Vector v Int, Vector v b)
                => (a -> b -> a) -> v a -> v Int -> v b -> v a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ f v is xs
  = unsafeAccum_stream f v (Bundle.zipWith (,) (stream is) (stream xs))

unsafeAccum_stream
  :: Vector v a => (a -> b -> a) -> v a -> Bundle u (Int,b) -> v a
{-# INLINE unsafeAccum_stream #-}
unsafeAccum_stream f = modifyWithBundle (M.unsafeAccum f)

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector
reverse :: (Vector v a) => v a -> v a
{-# INLINE reverse #-}
-- FIXME: make this fuse better, add support for recycling
reverse = unstream . streamR

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: (Vector v a, Vector v Int)
            => v a   -- ^ @xs@ value vector
            -> v Int -- ^ @is@ index vector (of length @n@)
            -> v a
{-# INLINE backpermute #-}
-- This somewhat non-intuitive definition ensures that the resulting vector
-- does not retain references to the original one even if it is lazy in its
-- elements. This would not be the case if we simply used map (v!)
backpermute v is = seq v
                 $ seq n
                 $ unstream
                 $ Bundle.unbox
                 $ Bundle.map index
                 $ stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = BOUNDS_CHECK(checkIndex) "backpermute" i n
            $ basicUnsafeIndexM v i

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: (Vector v a, Vector v Int) => v a -> v Int -> v a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute v is = seq v
                       $ seq n
                       $ unstream
                       $ Bundle.unbox
                       $ Bundle.map index
                       $ stream is
  where
    n = length v

    {-# INLINE index #-}
    -- NOTE: we do it this way to avoid triggering LiberateCase on n in
    -- polymorphic code
    index i = UNSAFE_CHECK(checkIndex) "unsafeBackpermute" i n
            $ basicUnsafeIndexM v i

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation will be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise.
--
-- @
-- modify (\\v -> 'M.write' v 0 \'x\') ('replicate' 3 \'a\') = \<\'x\',\'a\',\'a\'\>
-- @
modify :: Vector v a => (forall s. Mutable v s a -> ST s ()) -> v a -> v a
{-# INLINE modify #-}
modify p = new . New.modify p . clone

-- We have to make sure that this is strict in the stream but we can't seq on
-- it while fusion is happening. Hence this ugliness.
modifyWithBundle :: Vector v a
                 => (forall s. Mutable v s a -> Bundle u b -> ST s ())
                 -> v a -> Bundle u b -> v a
{-# INLINE modifyWithBundle #-}
modifyWithBundle p v s = new (New.modifyWithBundle p (clone v) s)

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index
indexed :: (Vector v a, Vector v (Int,a)) => v a -> v (Int,a)
{-# INLINE indexed #-}
indexed = unstream . Bundle.indexed . stream

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = unstream . inplace (S.map f) id . stream

-- | /O(n)/ Apply a function to every element of a vector and its index
imap :: (Vector v a, Vector v b) => (Int -> a -> b) -> v a -> v b
{-# INLINE imap #-}
imap f = unstream . inplace (S.map (uncurry f) . S.indexed) id
                  . stream

-- | Map a function over a vector and concatenate the results.
concatMap :: (Vector v a, Vector v b) => (a -> v b) -> v a -> v b
{-# INLINE concatMap #-}
-- NOTE: We can't fuse concatMap anyway so don't pretend we do.
-- This seems to be slightly slower
-- concatMap f = concat . Bundle.toList . Bundle.map f . stream

-- Slowest
-- concatMap f = unstream . Bundle.concatMap (stream . f) . stream

-- Used to be fastest
{-
concatMap f = unstream
            . Bundle.flatten mk step Unknown
            . stream
  where
    {-# INLINE_INNER step #-}
    step (v,i,k)
      | i < k = case unsafeIndexM v i of
                  Box x -> Bundle.Yield x (v,i+1,k)
      | otherwise = Bundle.Done

    {-# INLINE mk #-}
    mk x = let v = f x
               k = length v
           in
           k `seq` (v,0,k)
-}

-- This seems to be fastest now
concatMap f = unstream
            . Bundle.concatVectors
            . Bundle.map f
            . stream

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results
mapM :: (Monad m, Vector v a, Vector v b) => (a -> m b) -> v a -> m (v b)
{-# INLINE mapM #-}
mapM f = unstreamM . Bundle.mapM f . stream

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, yielding a vector of results
imapM :: (Monad m, Vector v a, Vector v b)
      => (Int -> a -> m b) -> v a -> m (v b)
imapM f = unstreamM . Bundle.mapM (uncurry f) . Bundle.indexed . stream

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results
mapM_ :: (Monad m, Vector v a) => (a -> m b) -> v a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = Bundle.mapM_ f . stream

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results
imapM_ :: (Monad m, Vector v a) => (Int -> a -> m b) -> v a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = Bundle.mapM_ (uncurry f) . Bundle.indexed . stream

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
forM :: (Monad m, Vector v a, Vector v b) => v a -> (a -> m b) -> m (v b)
{-# INLINE forM #-}
forM as f = mapM f as

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Vector v a) => v a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ as f = mapM_ f as

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices, yielding a
-- vector of results. Equivalent to 'flip' 'imapM'.
--
-- @since 0.12.2.0
iforM :: (Monad m, Vector v a, Vector v b) => v a -> (Int -> a -> m b) -> m (v b)
{-# INLINE iforM #-}
iforM as f = imapM f as

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices and ignore the
-- results. Equivalent to 'flip' 'imapM_'.
--
-- @since 0.12.2.0
iforM_ :: (Monad m, Vector v a) => v a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ as f = imapM_ f as

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f = \xs ys -> unstream (Bundle.zipWith f (stream xs) (stream ys))

-- | Zip three vectors with the given function.
zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d)
         => (a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE zipWith3 #-}
zipWith3 f = \as bs cs -> unstream (Bundle.zipWith3 f (stream as)
                                                  (stream bs)
                                                  (stream cs))

zipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
         => (a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
{-# INLINE zipWith4 #-}
zipWith4 f = \as bs cs ds ->
    unstream (Bundle.zipWith4 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds))

zipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f)
         => (a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d -> v e
                                         -> v f
{-# INLINE zipWith5 #-}
zipWith5 f = \as bs cs ds es ->
    unstream (Bundle.zipWith5 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es))

zipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f, Vector v g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
{-# INLINE zipWith6 #-}
zipWith6 f = \as bs cs ds es fs ->
    unstream (Bundle.zipWith6 f (stream as)
                                (stream bs)
                                (stream cs)
                                (stream ds)
                                (stream es)
                                (stream fs))

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Vector v a, Vector v b, Vector v c)
        => (Int -> a -> b -> c) -> v a -> v b -> v c
{-# INLINE izipWith #-}
izipWith f = \xs ys ->
    unstream (Bundle.zipWith (uncurry f) (Bundle.indexed (stream xs))
                                                         (stream ys))

izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d)
         => (Int -> a -> b -> c -> d) -> v a -> v b -> v c -> v d
{-# INLINE izipWith3 #-}
izipWith3 f = \as bs cs ->
    unstream (Bundle.zipWith3 (uncurry f) (Bundle.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs))

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e)
         => (Int -> a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
{-# INLINE izipWith4 #-}
izipWith4 f = \as bs cs ds ->
    unstream (Bundle.zipWith4 (uncurry f) (Bundle.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds))

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f)
         => (Int -> a -> b -> c -> d -> e -> f) -> v a -> v b -> v c -> v d
                                                -> v e -> v f
{-# INLINE izipWith5 #-}
izipWith5 f = \as bs cs ds es ->
    unstream (Bundle.zipWith5 (uncurry f) (Bundle.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es))

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
             Vector v f, Vector v g)
         => (Int -> a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
{-# INLINE izipWith6 #-}
izipWith6 f = \as bs cs ds es fs ->
    unstream (Bundle.zipWith6 (uncurry f) (Bundle.indexed (stream as))
                                                          (stream bs)
                                                          (stream cs)
                                                          (stream ds)
                                                          (stream es)
                                                          (stream fs))

-- | /O(min(m,n))/ Zip two vectors
zip :: (Vector v a, Vector v b, Vector v (a,b)) => v a -> v b -> v (a, b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
     => v a -> v b -> v c -> v (a, b, c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d))
     => v a -> v b -> v c -> v d -> v (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v (a, b, c, d, e))
     => v a -> v b -> v c -> v d -> v e -> v (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
         Vector v f, Vector v (a, b, c, d, e, f))
     => v a -> v b -> v c -> v d -> v e -> v f -> v (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results
zipWithM :: (Monad m, Vector v a, Vector v b, Vector v c)
         => (a -> b -> m c) -> v a -> v b -> m (v c)
-- FIXME: specialise for ST and IO?
{-# INLINE zipWithM #-}
zipWithM f = \as bs -> unstreamM $ Bundle.zipWithM f (stream as) (stream bs)

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and yield a vector of results
izipWithM :: (Monad m, Vector v a, Vector v b, Vector v c)
         => (Int -> a -> b -> m c) -> v a -> v b -> m (v c)
{-# INLINE izipWithM #-}
izipWithM m as bs = unstreamM . Bundle.zipWithM (uncurry m)
                                (Bundle.indexed (stream as))
                                $ stream bs

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results
zipWithM_ :: (Monad m, Vector v a, Vector v b)
          => (a -> b -> m c) -> v a -> v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f = \as bs -> Bundle.zipWithM_ f (stream as) (stream bs)

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and ignore the results
izipWithM_ :: (Monad m, Vector v a, Vector v b)
          => (Int -> a -> b -> m c) -> v a -> v b -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ m as bs = Bundle.zipWithM_ (uncurry m)
                      (Bundle.indexed (stream as))
                      $ stream bs

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
unzip :: (Vector v a, Vector v b, Vector v (a,b)) => v (a, b) -> (v a, v b)
{-# INLINE unzip #-}
unzip xs = (map fst xs, map snd xs)

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c))
       => v (a, b, c) -> (v a, v b, v c)
{-# INLINE unzip3 #-}
unzip3 xs = (map (\(a, _, _) -> a) xs,
             map (\(_, b, _) -> b) xs,
             map (\(_, _, c) -> c) xs)

unzip4 :: (Vector v a, Vector v b, Vector v c, Vector v d,
           Vector v (a, b, c, d))
       => v (a, b, c, d) -> (v a, v b, v c, v d)
{-# INLINE unzip4 #-}
unzip4 xs = (map (\(a, _, _, _) -> a) xs,
             map (\(_, b, _, _) -> b) xs,
             map (\(_, _, c, _) -> c) xs,
             map (\(_, _, _, d) -> d) xs)

unzip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v (a, b, c, d, e))
       => v (a, b, c, d, e) -> (v a, v b, v c, v d, v e)
{-# INLINE unzip5 #-}
unzip5 xs = (map (\(a, _, _, _, _) -> a) xs,
             map (\(_, b, _, _, _) -> b) xs,
             map (\(_, _, c, _, _) -> c) xs,
             map (\(_, _, _, d, _) -> d) xs,
             map (\(_, _, _, _, e) -> e) xs)

unzip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e,
           Vector v f, Vector v (a, b, c, d, e, f))
       => v (a, b, c, d, e, f) -> (v a, v b, v c, v d, v e, v f)
{-# INLINE unzip6 #-}
unzip6 xs = (map (\(a, _, _, _, _, _) -> a) xs,
             map (\(_, b, _, _, _, _) -> b) xs,
             map (\(_, _, c, _, _, _) -> c) xs,
             map (\(_, _, _, d, _, _) -> d) xs,
             map (\(_, _, _, _, e, _) -> e) xs,
             map (\(_, _, _, _, _, f) -> f) xs)

-- Filtering
-- ---------

-- | /O(n)/ Drop elements that do not satisfy the predicate
filter :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE filter #-}
filter f = unstream . inplace (S.filter f) toMax . stream

-- | /O(n)/ Drop elements that do not satisfy the predicate which is applied to
-- values and their indices
ifilter :: Vector v a => (Int -> a -> Bool) -> v a -> v a
{-# INLINE ifilter #-}
ifilter f = unstream
          . inplace (S.map snd . S.filter (uncurry f) . S.indexed) toMax
          . stream

-- | /O(n)/ Drop repeated adjacent elements.
uniq :: (Vector v a, Eq a) => v a -> v a
{-# INLINE uniq #-}
uniq = unstream . inplace S.uniq toMax . stream

-- | /O(n)/ Drop elements when predicate returns Nothing
mapMaybe :: (Vector v a, Vector v b) => (a -> Maybe b) -> v a -> v b
{-# INLINE mapMaybe #-}
mapMaybe f = unstream . inplace (S.mapMaybe f) toMax . stream

-- | /O(n)/ Drop elements when predicate, applied to index and value, returns Nothing
imapMaybe :: (Vector v a, Vector v b) => (Int -> a -> Maybe b) -> v a -> v b
{-# INLINE imapMaybe #-}
imapMaybe f = unstream
          . inplace (S.mapMaybe (uncurry f) . S.indexed) toMax
          . stream


-- | /O(n)/ Drop elements that do not satisfy the monadic predicate
filterM :: (Monad m, Vector v a) => (a -> m Bool) -> v a -> m (v a)
{-# INLINE filterM #-}
filterM f = unstreamM . Bundle.filterM f . stream

-- | /O(n)/ Apply monadic function to each element of vector and
-- discard elements returning Nothing.
--
-- @since 0.12.2.0
mapMaybeM :: (Monad m, Vector v a, Vector v b) => (a -> m (Maybe b)) -> v a -> m (v b)
{-# INLINE mapMaybeM #-}
mapMaybeM f = unstreamM . Bundle.mapMaybeM f . stream

-- | /O(n)/ Apply monadic function to each element of vector and its index.
-- Discards elements returning Nothing.
--
-- @since 0.12.2.0
imapMaybeM :: (Monad m, Vector v a, Vector v b)
      => (Int -> a -> m (Maybe b)) -> v a -> m (v b)
{-# INLINE imapMaybeM #-}
imapMaybeM f = unstreamM . Bundle.mapMaybeM (\(i, a) -> f i a) . Bundle.indexed . stream

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- Current implementation is not copy-free, unless the result vector is
-- fused away.
takeWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE takeWhile #-}
takeWhile f = unstream . Bundle.takeWhile f . stream

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: Vector v a => (a -> Bool) -> v a -> v a
{-# INLINE_FUSED dropWhile #-}
-- In the case that the argument is an actual vector,
-- this is a faster solution than stream fusion.
dropWhile f xs = case findIndex (not . f) xs of
                   Just i  -> unsafeDrop i xs
                   Nothing -> empty

-- If we have optimization turned on
-- and the argument to 'dropWhile' comes from a stream,
-- we never allocate the argument vector, and
-- whenever possible, we avoid creating the resulting vector actually in heap.
--
-- Also note that @'new' . 'New.unstream'@
-- is the definition (to be @INLINE@d) of 'unstream'.
{-# RULES
"dropWhile/unstream [Vector]" forall f p.
  dropWhile f (new (New.unstream p)) = new (New.unstream (Bundle.dropWhile f p))
  #-}

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE partition #-}
partition f = partition_stream f . stream

-- FIXME: Make this inplace-fusible (look at how stable_partition is
-- implemented in C++)

partition_stream :: Vector v a => (a -> Bool) -> Bundle u a -> (v a, v a)
{-# INLINE_FUSED partition_stream #-}
partition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.partitionBundle f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

-- | /O(n)/ Split the vector into two parts, the first one containing the
-- @`Left`@ elements and the second containing the @`Right`@ elements.
-- The relative order of the elements is preserved.
--
-- @since 0.12.1.0
partitionWith :: (Vector v a, Vector v b, Vector v c) => (a -> Either b c) -> v a -> (v b, v c)
{-# INLINE partitionWith #-}
partitionWith f = partition_with_stream f . stream

partition_with_stream :: (Vector v a, Vector v b, Vector v c) => (a -> Either b c) -> Bundle u a -> (v b, v c)
{-# INLINE_FUSED partition_with_stream #-}
partition_with_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.partitionWithBundle f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved but the operation is often
-- faster than 'partition'.
unstablePartition :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE unstablePartition #-}
unstablePartition f = unstablePartition_stream f . stream

unstablePartition_stream
  :: Vector v a => (a -> Bool) -> Bundle u a -> (v a, v a)
{-# INLINE_FUSED unstablePartition_stream #-}
unstablePartition_stream f s = s `seq` runST (
  do
    (mv1,mv2) <- M.unstablePartitionBundle f s
    v1 <- unsafeFreeze mv1
    v2 <- unsafeFreeze mv2
    return (v1,v2))

unstablePartition_new :: Vector v a => (a -> Bool) -> New v a -> (v a, v a)
{-# INLINE_FUSED unstablePartition_new #-}
unstablePartition_new f (New.New p) = runST (
  do
    mv <- p
    i <- M.unstablePartition f mv
    v <- unsafeFreeze mv
    return (unsafeTake i v, unsafeDrop i v))

{-# RULES

"unstablePartition" forall f p.
  unstablePartition_stream f (stream (new p))
    = unstablePartition_new f p   #-}




-- FIXME: make span and break fusible

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
span :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE span #-}
span f = break (not . f)

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
break :: Vector v a => (a -> Bool) -> v a -> (v a, v a)
{-# INLINE break #-}
break f xs = case findIndex f xs of
               Just i  -> (unsafeSlice 0 i xs, unsafeSlice i (length xs - i) xs)
               Nothing -> (xs, empty)


-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element
elem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE elem #-}
elem x = Bundle.elem x . stream

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem')
notElem :: (Vector v a, Eq a) => a -> v a -> Bool
{-# INLINE notElem #-}
notElem x = Bundle.notElem x . stream

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Vector v a => (a -> Bool) -> v a -> Maybe a
{-# INLINE find #-}
find f = Bundle.find f . stream

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Vector v a => (a -> Bool) -> v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = Bundle.findIndex f . stream

-- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- or 'Nothing' if no such element exists.
--
-- @since 0.12.2.0
findIndexR :: Vector v a => (a -> Bool) -> v a -> Maybe Int
{-# INLINE findIndexR #-}
findIndexR f v = fmap (length v - 1 -) . Bundle.findIndex f $ streamR v

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (Vector v a, Vector v Int) => (a -> Bool) -> v a -> v Int
{-# INLINE findIndices #-}
findIndices f = unstream
              . inplace (S.map fst . S.filter (f . snd) . S.indexed) toMax
              . stream

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Vector v a, Eq a) => a -> v a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex x = findIndex (x==)

-- | /O(n)/ Yield the indices of all occurences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: (Vector v a, Vector v Int, Eq a) => a -> v a -> v Int
{-# INLINE elemIndices #-}
elemIndices x = findIndices (x==)

-- Folding
-- -------

-- | /O(n)/ Left fold
foldl :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl #-}
foldl f z = Bundle.foldl f z . stream

-- | /O(n)/ Left fold on non-empty vectors
foldl1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = Bundle.foldl1 f . stream

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Vector v b => (a -> b -> a) -> a -> v b -> a
{-# INLINE foldl' #-}
foldl' f z = Bundle.foldl' f z . stream

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator
foldl1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldl1' #-}
foldl1' f = Bundle.foldl1' f . stream

-- | /O(n)/ Right fold
foldr :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f z = Bundle.foldr f z . stream

-- | /O(n)/ Right fold on non-empty vectors
foldr1 :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1 #-}
foldr1 f = Bundle.foldr1 f . stream

-- | /O(n)/ Right fold with a strict accumulator
foldr' :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr' #-}
foldr' f z = Bundle.foldl' (flip f) z . streamR

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator
foldr1' :: Vector v a => (a -> a -> a) -> v a -> a
{-# INLINE foldr1' #-}
foldr1' f = Bundle.foldl1' (flip f) . streamR

-- | /O(n)/ Left fold (function applied to each element and its index)
ifoldl :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl #-}
ifoldl f z = Bundle.foldl (uncurry . f) z . Bundle.indexed . stream

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index)
ifoldl' :: Vector v b => (a -> Int -> b -> a) -> a -> v b -> a
{-# INLINE ifoldl' #-}
ifoldl' f z = Bundle.foldl' (uncurry . f) z . Bundle.indexed . stream

-- | /O(n)/ Right fold (function applied to each element and its index)
ifoldr :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr #-}
ifoldr f z = Bundle.foldr (uncurry f) z . Bundle.indexed . stream

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index)
ifoldr' :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr' #-}
ifoldr' f z xs = Bundle.foldl' (flip (uncurry f)) z
               $ Bundle.indexedR (length xs) $ streamR xs

-- | /O(n)/ Map each element of the structure to a monoid, and combine
-- the results. It uses same implementation as corresponding method of
-- 'Foldable' type cless. Note it's implemented in terms of 'foldr'
-- and won't fuse with functions that traverse vector from left to
-- right ('map', 'generate', etc.).
--
-- @since 0.12.2.0
foldMap :: (Monoid m, Vector v a) => (a -> m) -> v a -> m
{-# INLINE foldMap #-}
foldMap f = foldr (mappend . f) mempty

-- | /O(n)/ 'foldMap' which is strict in accumulator. It uses same
-- implementation as corresponding method of 'Foldable' type class.
-- Note it's implemented in terms of 'foldl'' so it fuses in most
-- contexts.
--
-- @since 0.12.2.0
foldMap' :: (Monoid m, Vector v a) => (a -> m) -> v a -> m
{-# INLINE foldMap' #-}
foldMap' f = foldl' (\acc a -> acc `mappend` f a) mempty


-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.all even $ V.fromList [2, 4, 12 :: Int]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13 :: Int]
-- False
-- >>> V.all even (V.empty :: V.Vector Int)
-- True
all :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE all #-}
all f = Bundle.and . Bundle.map f . stream

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.any even $ V.fromList [1, 3, 7 :: Int]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13 :: Int]
-- True
-- >>> V.any even (V.empty :: V.Vector Int)
-- False
any :: Vector v a => (a -> Bool) -> v a -> Bool
{-# INLINE any #-}
any f = Bundle.or . Bundle.map f . stream

-- | /O(n)/ Check if all elements are 'True'
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
and :: Vector v Bool => v Bool -> Bool
{-# INLINE and #-}
and = Bundle.and . stream

-- | /O(n)/ Check if any element is 'True'
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
or :: Vector v Bool => v Bool -> Bool
{-# INLINE or #-}
or = Bundle.or . stream

-- | /O(n)/ Compute the sum of the elements
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.sum $ V.fromList [300,20,1 :: Int]
-- 321
-- >>> V.sum (V.empty :: V.Vector Int)
-- 0
sum :: (Vector v a, Num a) => v a -> a
{-# INLINE sum #-}
sum = Bundle.foldl' (+) 0 . stream

-- | /O(n)/ Compute the produce of the elements
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.product $ V.fromList [1,2,3,4 :: Int]
-- 24
-- >>> V.product (V.empty :: V.Vector Int)
-- 1
product :: (Vector v a, Num a) => v a -> a
{-# INLINE product #-}
product = Bundle.foldl' (*) 1 . stream

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximum $ V.fromList [2.0, 1.0]
-- 2.0
maximum :: (Vector v a, Ord a) => v a -> a
{-# INLINE maximum #-}
maximum = Bundle.foldl1' max . stream

-- | /O(n)/ Yield the maximum element of the vector according to the given
-- comparison function. The vector may not be empty.
maximumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE maximumBy #-}
maximumBy cmpr = Bundle.foldl1' maxBy . stream
  where
    {-# INLINE maxBy #-}
    maxBy x y = case cmpr x y of
                  LT -> y
                  _  -> x

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimum $ V.fromList [2.0, 1.0]
-- 1.0
minimum :: (Vector v a, Ord a) => v a -> a
{-# INLINE minimum #-}
minimum = Bundle.foldl1' min . stream

-- | /O(n)/ Yield the minimum element of the vector according to the given
-- comparison function. The vector may not be empty.
minimumBy :: Vector v a => (a -> a -> Ordering) -> v a -> a
{-# INLINE minimumBy #-}
minimumBy cmpr = Bundle.foldl1' minBy . stream
  where
    {-# INLINE minBy #-}
    minBy x y = case cmpr x y of
                  GT -> y
                  _  -> x

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE maxIndex #-}
maxIndex = maxIndexBy compare

-- | /O(n)/ Yield the index of the maximum element of the vector according to
-- the given comparison function. The vector may not be empty.
maxIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy cmpr = fst . Bundle.foldl1' imax . Bundle.indexed . stream
  where
    imax (i,x) (j,y) = i `seq` j `seq`
                       case cmpr x y of
                         LT -> (j,y)
                         _  -> (i,x)

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: (Vector v a, Ord a) => v a -> Int
{-# INLINE minIndex #-}
minIndex = minIndexBy compare

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
minIndexBy :: Vector v a => (a -> a -> Ordering) -> v a -> Int
{-# INLINE minIndexBy #-}
minIndexBy cmpr = fst . Bundle.foldl1' imin . Bundle.indexed . stream
  where
    imin (i,x) (j,y) = i `seq` j `seq`
                       case cmpr x y of
                         GT -> (j,y)
                         _  -> (i,x)

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold
foldM :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m a
{-# INLINE foldM #-}
foldM m z = Bundle.foldM m z . stream

-- | /O(n)/ Monadic fold (action applied to each element and its index)
ifoldM :: (Monad m, Vector v b) => (a -> Int -> b -> m a) -> a -> v b -> m a
{-# INLINE ifoldM #-}
ifoldM m z = Bundle.foldM (uncurry . m) z . Bundle.indexed . stream

-- | /O(n)/ Monadic fold over non-empty vectors
fold1M :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m a
{-# INLINE fold1M #-}
fold1M m = Bundle.fold1M m . stream

-- | /O(n)/ Monadic fold with strict accumulator
foldM' :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m a
{-# INLINE foldM' #-}
foldM' m z = Bundle.foldM' m z . stream

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each
-- element and its index)
ifoldM' :: (Monad m, Vector v b) => (a -> Int -> b -> m a) -> a -> v b -> m a
{-# INLINE ifoldM' #-}
ifoldM' m z = Bundle.foldM' (uncurry . m) z . Bundle.indexed . stream

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
fold1M' :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m a
{-# INLINE fold1M' #-}
fold1M' m = Bundle.fold1M' m . stream

discard :: Monad m => m a -> m ()
{-# INLINE discard #-}
discard m = m >> return ()

-- | /O(n)/ Monadic fold that discards the result
foldM_ :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m ()
{-# INLINE foldM_ #-}
foldM_ m z = discard . Bundle.foldM m z . stream

-- | /O(n)/ Monadic fold that discards the result (action applied to
-- each element and its index)
ifoldM_ :: (Monad m, Vector v b) => (a -> Int -> b -> m a) -> a -> v b -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ m z = discard . Bundle.foldM (uncurry . m) z . Bundle.indexed . stream

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result
fold1M_ :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ m = discard . Bundle.fold1M m . stream

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
foldM'_ :: (Monad m, Vector v b) => (a -> b -> m a) -> a -> v b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ m z = discard . Bundle.foldM' m z . stream

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- (action applied to each element and its index)
ifoldM'_ :: (Monad m, Vector v b) => (a -> Int -> b -> m a) -> a -> v b -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ m z = discard . Bundle.foldM' (uncurry . m) z . Bundle.indexed . stream

-- | /O(n)/ Monad fold over non-empty vectors with strict accumulator
-- that discards the result
fold1M'_ :: (Monad m, Vector v a) => (a -> a -> m a) -> v a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ m = discard . Bundle.fold1M' m . stream

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results
sequence :: (Monad m, Vector v a, Vector v (m a)) => v (m a) -> m (v a)
{-# INLINE sequence #-}
sequence = mapM id

-- | Evaluate each action and discard the results
sequence_ :: (Monad m, Vector v (m a)) => v (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = mapM_ id

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
prescanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl #-}
prescanl f z = unstream . inplace (S.prescanl f z) id . stream

-- | /O(n)/ Prescan with strict accumulator
prescanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE prescanl' #-}
prescanl' f z = unstream . inplace (S.prescanl' f z) id . stream

-- | /O(n)/ Scan
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- Example: @postscanl (+) 0 \<1,2,3,4\> = \<1,3,6,10\>@
--
postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl #-}
postscanl f z = unstream . inplace (S.postscanl f z) id . stream

-- | /O(n)/ Scan with strict accumulator
postscanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE postscanl' #-}
postscanl' f z = unstream . inplace (S.postscanl' f z) id . stream

-- | /O(n)/ Haskell-style scan
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- Example: @scanl (+) 0 \<1,2,3,4\> = \<0,1,3,6,10\>@
--
scanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl #-}
scanl f z = unstream . Bundle.scanl f z . stream

-- | /O(n)/ Haskell-style scan with strict accumulator
scanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> v b -> v a
{-# INLINE scanl' #-}
scanl' f z = unstream . Bundle.scanl' f z . stream

-- | /O(n)/ Scan over a vector with its index
iscanl :: (Vector v a, Vector v b) => (Int -> a -> b -> a) -> a -> v b -> v a
{-# INLINE iscanl #-}
iscanl f z =
    unstream
  . inplace (S.scanl (\a (i, b) -> f i a b) z . S.indexed) (+1)
  . stream

-- | /O(n)/ Scan over a vector (strictly) with its index
iscanl' :: (Vector v a, Vector v b) => (Int -> a -> b -> a) -> a -> v b -> v a
{-# INLINE iscanl' #-}
iscanl' f z =
    unstream
  . inplace (S.scanl' (\a (i, b) -> f i a b) z . S.indexed) (+1)
  . stream


-- | /O(n)/ Scan over a non-empty vector
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
scanl1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1 #-}
scanl1 f = unstream . inplace (S.scanl1 f) id . stream

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator
scanl1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanl1' #-}
scanl1' f = unstream . inplace (S.scanl1' f) id . stream

-- | /O(n)/ Right-to-left prescan
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
--
prescanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr #-}
prescanr f z = unstreamR . inplace (S.prescanl (flip f) z) id . streamR

-- | /O(n)/ Right-to-left prescan with strict accumulator
prescanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE prescanr' #-}
prescanr' f z = unstreamR . inplace (S.prescanl' (flip f) z) id . streamR

-- | /O(n)/ Right-to-left scan
postscanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr #-}
postscanr f z = unstreamR . inplace (S.postscanl (flip f) z) id . streamR

-- | /O(n)/ Right-to-left scan with strict accumulator
postscanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE postscanr' #-}
postscanr' f z = unstreamR . inplace (S.postscanl' (flip f) z) id . streamR

-- | /O(n)/ Right-to-left Haskell-style scan
scanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr #-}
scanr f z = unstreamR . Bundle.scanl (flip f) z . streamR

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator
scanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> v a -> v b
{-# INLINE scanr' #-}
scanr' f z = unstreamR . Bundle.scanl' (flip f) z . streamR

-- | /O(n)/ Right-to-left scan over a vector with its index
iscanr :: (Vector v a, Vector v b) => (Int -> a -> b -> b) -> b -> v a -> v b
{-# INLINE iscanr #-}
iscanr f z v =
    unstreamR
  . inplace (S.scanl (flip $ uncurry f) z . S.indexedR n) (+1)
  . streamR
  $ v
 where n = length v

-- | /O(n)/ Right-to-left scan over a vector (strictly) with its index
iscanr' :: (Vector v a, Vector v b) => (Int -> a -> b -> b) -> b -> v a -> v b
{-# INLINE iscanr' #-}
iscanr' f z v =
    unstreamR
  . inplace (S.scanl' (flip $ uncurry f) z . S.indexedR n) (+1)
  . streamR
  $ v
 where n = length v

-- | /O(n)/ Right-to-left scan over a non-empty vector
scanr1 :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1 #-}
scanr1 f = unstreamR . inplace (S.scanl1 (flip f)) id . streamR

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator
scanr1' :: Vector v a => (a -> a -> a) -> v a -> v a
{-# INLINE scanr1' #-}
scanr1' f = unstreamR . inplace (S.scanl1' (flip f)) id . streamR

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list
toList :: Vector v a => v a -> [a]
{-# INLINE toList #-}
toList = Bundle.toList . stream

-- | /O(n)/ Convert a list to a vector
fromList :: Vector v a => [a] -> v a
{-# INLINE fromList #-}
fromList = unstream . Bundle.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.fromListN 3 [1,2,3,4,5::Int]
-- [1,2,3]
-- >>> V.fromListN 3 [1::Int]
-- [1]
fromListN :: Vector v a => Int -> [a] -> v a
{-# INLINE fromListN #-}
fromListN n = unstream . Bundle.fromListN n

-- Conversions - Immutable vectors
-- -------------------------------

-- | /O(n)/ Convert different vector types
convert :: (Vector v a, Vector w a) => v a -> w a
{-# INLINE convert #-}
convert = unstream . Bundle.reVector . stream

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafe convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = basicUnsafeFreeze

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE freeze #-}
freeze mv = unsafeFreeze =<< M.clone mv

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw :: (PrimMonad m, Vector v a) => v a -> m (Mutable v (PrimState m) a)
{-# INLINE_FUSED unsafeThaw #-}
unsafeThaw = basicUnsafeThaw

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (PrimMonad m, Vector v a) => v a -> m (Mutable v (PrimState m) a)
{-# INLINE_FUSED thaw #-}
thaw v = do
           mv <- M.unsafeNew (length v)
           unsafeCopy mv v
           return mv

{-# RULES

"unsafeThaw/new [Vector]" forall p.
  unsafeThaw (new p) = New.runPrim p

"thaw/new [Vector]" forall p.
  thaw (new p) = New.runPrim p   #-}



{-
-- | /O(n)/ Yield a mutable vector containing copies of each vector in the
-- list.
thawMany :: (PrimMonad m, Vector v a) => [v a] -> m (Mutable v (PrimState m) a)
{-# INLINE_FUSED thawMany #-}
-- FIXME: add rule for (stream (new (New.create (thawMany vs))))
-- NOTE: We don't try to consume the list lazily as this wouldn't significantly
-- change the space requirements anyway.
thawMany vs = do
                mv <- M.new n
                thaw_loop mv vs
                return mv
  where
    n = List.foldl' (\k v -> k + length v) 0 vs

    thaw_loop mv [] = mv `seq` return ()
    thaw_loop mv (v:vs)
      = do
          let n = length v
          unsafeCopy (M.unsafeTake n mv) v
          thaw_loop (M.unsafeDrop n mv) vs
-}

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> v a -> m ()
{-# INLINE copy #-}
copy dst src = BOUNDS_CHECK(check) "copy" "length mismatch"
                                          (M.length dst == basicLength src)
             $ unsafeCopy dst src

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy
  :: (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> v a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst src = UNSAFE_CHECK(check) "unsafeCopy" "length mismatch"
                                         (M.length dst == basicLength src)
                   $ (dst `seq` src `seq` basicUnsafeCopy dst src)

-- Conversions to/from Bundles
-- ---------------------------

-- | /O(1)/ Convert a vector to a 'Bundle'
stream :: Vector v a => v a -> Bundle v a
{-# INLINE_FUSED stream #-}
stream v = Bundle.fromVector v

{-
stream v = v `seq` n `seq` (Bundle.unfoldr get 0 `Bundle.sized` Exact n)
  where
    n = length v

    -- NOTE: the False case comes first in Core so making it the recursive one
    -- makes the code easier to read
    {-# INLINE get #-}
    get i | i >= n    = Nothing
          | otherwise = case basicUnsafeIndexM v i of Box x -> Just (x, i+1)
-}

-- | /O(n)/ Construct a vector from a 'Bundle'
unstream :: Vector v a => Bundle v a -> v a
{-# INLINE unstream #-}
unstream s = new (New.unstream s)

{-# RULES

"stream/unstream [Vector]" forall s.
  stream (new (New.unstream s)) = s

"New.unstream/stream [Vector]" forall v.
  New.unstream (stream v) = clone v

"clone/new [Vector]" forall p.
  clone (new p) = p

"inplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  New.unstream (inplace f g (stream (new m))) = New.transform f g m

"uninplace [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  stream (new (New.transform f g m)) = inplace f g (stream (new m))  #-}



-- | /O(1)/ Convert a vector to a 'Bundle', proceeding from right to left
streamR :: Vector v a => v a -> Bundle u a
{-# INLINE_FUSED streamR #-}
streamR v = v `seq` n `seq` (Bundle.unfoldr get n `Bundle.sized` Exact n)
  where
    n = length v

    {-# INLINE get #-}
    get 0 = Nothing
    get i = let i' = i-1
            in
            case basicUnsafeIndexM v i' of Box x -> Just (x, i')

-- | /O(n)/ Construct a vector from a 'Bundle', proceeding from right to left
unstreamR :: Vector v a => Bundle v a -> v a
{-# INLINE unstreamR #-}
unstreamR s = new (New.unstreamR s)

{-# RULES

"streamR/unstreamR [Vector]" forall s.
  streamR (new (New.unstreamR s)) = s

"New.unstreamR/streamR/new [Vector]" forall p.
  New.unstreamR (streamR (new p)) = p

"New.unstream/streamR/new [Vector]" forall p.
  New.unstream (streamR (new p)) = New.modify M.reverse p

"New.unstreamR/stream/new [Vector]" forall p.
  New.unstreamR (stream (new p)) = New.modify M.reverse p

"inplace right [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  New.unstreamR (inplace f g (streamR (new m))) = New.transformR f g m

"uninplace right [Vector]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a) g m.
  streamR (new (New.transformR f g m)) = inplace f g (streamR (new m))  #-}


-- | Load monadic stream bundle into a newly allocated vector. This function goes through
-- a list, so prefer using `unstream`, unless you need to be in a monad.
--
-- @since 0.12.2.0
unstreamM :: (Monad m, Vector v a) => MBundle m u a -> m (v a)
{-# INLINE_FUSED unstreamM #-}
unstreamM s = do
                xs <- MBundle.toList s
                return $ unstream $ Bundle.unsafeFromList (MBundle.size s) xs

unstreamPrimM :: (PrimMonad m, Vector v a) => MBundle m u a -> m (v a)
{-# INLINE_FUSED unstreamPrimM #-}
unstreamPrimM s = M.munstream s >>= unsafeFreeze

-- FIXME: the next two functions are only necessary for the specialisations
unstreamPrimM_IO :: Vector v a => MBundle IO u a -> IO (v a)
{-# INLINE unstreamPrimM_IO #-}
unstreamPrimM_IO = unstreamPrimM

unstreamPrimM_ST :: Vector v a => MBundle (ST s) u a -> ST s (v a)
{-# INLINE unstreamPrimM_ST #-}
unstreamPrimM_ST = unstreamPrimM

{-# RULES

"unstreamM[IO]" unstreamM = unstreamPrimM_IO
"unstreamM[ST]" unstreamM = unstreamPrimM_ST  #-}




-- Recycling support
-- -----------------

-- | Construct a vector from a monadic initialiser.
new :: Vector v a => New v a -> v a
{-# INLINE_FUSED new #-}
new m = m `seq` runST (unsafeFreeze =<< New.run m)

-- | Convert a vector to an initialiser which, when run, produces a copy of
-- the vector.
clone :: Vector v a => v a -> New v a
{-# INLINE_FUSED clone #-}
clone v = v `seq` New.create (
  do
    mv <- M.new (basicLength v)
    unsafeCopy mv v
    return mv)

-- Comparisons
-- -----------

-- | /O(n)/ Check if two vectors are equal. All 'Vector' instances are also
-- instances of 'Eq' and it is usually more appropriate to use those. This
-- function is primarily intended for implementing 'Eq' instances for new
-- vector types.
eq :: (Vector v a, Eq a) => v a -> v a -> Bool
{-# INLINE eq #-}
xs `eq` ys = stream xs == stream ys

-- | /O(n)/ Check if two vectors are equal using supplied equality
-- predicate.
eqBy :: (Vector v a, Vector v b) => (a -> b -> Bool) -> v a -> v b -> Bool
{-# INLINE eqBy #-}
eqBy e xs ys = Bundle.eqBy e (stream xs) (stream ys)

-- | /O(n)/ Compare two vectors lexicographically. All 'Vector' instances are
-- also instances of 'Ord' and it is usually more appropriate to use those. This
-- function is primarily intended for implementing 'Ord' instances for new
-- vector types.
cmp :: (Vector v a, Ord a) => v a -> v a -> Ordering
{-# INLINE cmp #-}
cmp xs ys = compare (stream xs) (stream ys)

-- | /O(n)/ Compare two vectors using supplied comparison function for
-- vector elements. Comparison works same as for lists.
--
-- > cmpBy compare == cmp
cmpBy :: (Vector v a, Vector v b) => (a -> b -> Ordering) -> v a -> v b -> Ordering
cmpBy c xs ys = Bundle.cmpBy c (stream xs) (stream ys)

-- Show
-- ----

-- | Generic definition of 'Prelude.showsPrec'
showsPrec :: (Vector v a, Show a) => Int -> v a -> ShowS
{-# INLINE showsPrec #-}
showsPrec _ = shows . toList

liftShowsPrec :: (Vector v a) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> v a -> ShowS
{-# INLINE liftShowsPrec #-}
liftShowsPrec _ s _ = s . toList

-- | Generic definition of 'Text.Read.readPrec'
readPrec :: (Vector v a, Read a) => Read.ReadPrec (v a)
{-# INLINE readPrec #-}
readPrec = do
  xs <- Read.readPrec
  return (fromList xs)

-- | /Note:/ uses 'ReadS'
liftReadsPrec :: (Vector v a) => (Int -> Read.ReadS a) -> ReadS [a] -> Int -> Read.ReadS (v a)
liftReadsPrec _ r _ s = [ (fromList v, s') | (v, s') <- r s ]

-- Data and Typeable
-- -----------------

-- | Generic definion of 'Data.Data.gfoldl' that views a 'Vector' as a
-- list.
gfoldl :: (Vector v a, Data a)
       => (forall d b. Data d => c (d -> b) -> d -> c b)
       -> (forall g. g -> c g)
       -> v a
       -> c (v a)
{-# INLINE gfoldl #-}
gfoldl f z v = z fromList `f` toList v

mkVecConstr :: String -> Constr
{-# INLINE mkVecConstr #-}
mkVecConstr name = mkConstr (mkVecType name) "fromList" [] Prefix

mkVecType :: String -> DataType
{-# INLINE mkVecType #-}
mkVecType name = mkDataType name [mkVecConstr name]

mkType :: String -> DataType
{-# INLINE mkType #-}
mkType = mkNoRepType

gunfold :: (Vector v a, Data a)
        => (forall b r. Data b => c (b -> r) -> c r)
        -> (forall r. r -> c r)
        -> Constr -> c (v a)
gunfold k z c = case constrIndex c of
  1 -> k (z fromList)
  _ -> error "gunfold"

#if __GLASGOW_HASKELL__ >= 707
dataCast :: (Vector v a, Data a, Typeable v, Typeable t)
#else
dataCast :: (Vector v a, Data a, Typeable1 v, Typeable1 t)
#endif
         => (forall d. Data  d => c (t d)) -> Maybe  (c (v a))
{-# INLINE dataCast #-}
dataCast f = gcast1 f

-- $setup
-- >>> :set -XFlexibleContexts
