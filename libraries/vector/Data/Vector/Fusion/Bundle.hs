{-# LANGUAGE CPP, FlexibleInstances, Rank2Types, BangPatterns #-}

-- |
-- Module      : Data.Vector.Fusion.Bundle
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Bundles for stream fusion
--

module Data.Vector.Fusion.Bundle (
  -- * Types
  Step(..), Chunk(..), Bundle, MBundle,

  -- * In-place markers
  inplace,

  -- * Size hints
  size, sized,

  -- * Length information
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, generate, (++),

  -- * Accessing individual elements
  head, last, (!!), (!?),

  -- * Substreams
  slice, init, tail, take, drop,

  -- * Mapping
  map, concatMap, flatten, unbox,

  -- * Zipping
  indexed, indexedR,
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- * Filtering
  filter, takeWhile, dropWhile,

  -- * Searching
  elem, notElem, find, findIndex,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1,

  -- * Specialised folds
  and, or,

  -- * Unfolding
  unfoldr, unfoldrN, unfoldrExactN, iterateN,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl',
  scanl1, scanl1',

  -- * Enumerations
  enumFromStepN, enumFromTo, enumFromThenTo,

  -- * Conversions
  toList, fromList, fromListN, unsafeFromList, lift,
  fromVector, reVector, fromVectors, concatVectors,

  -- * Monadic combinators
  mapM, mapM_, zipWithM, zipWithM_, filterM, mapMaybeM, foldM, fold1M, foldM', fold1M',

  eq, cmp, eqBy, cmpBy
) where

import Data.Vector.Generic.Base ( Vector )
import Data.Vector.Fusion.Bundle.Size
import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Bundle.Monadic ( Chunk(..), lift )
import qualified Data.Vector.Fusion.Bundle.Monadic as M
import qualified Data.Vector.Fusion.Stream.Monadic as S

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
#endif

import GHC.Base ( build )

-- Data.Vector.Internal.Check is unused
#define NOT_VECTOR_MODULE
#include "vector.h"

-- | The type of pure streams
type Bundle = M.Bundle Id

-- | Alternative name for monadic streams
type MBundle = M.Bundle

inplace :: (forall m. Monad m => S.Stream m a -> S.Stream m b)
        -> (Size -> Size) -> Bundle v a -> Bundle v b
{-# INLINE_FUSED inplace #-}
inplace f g b = b `seq` M.fromStream (f (M.elements b)) (g (M.size b))

{-# RULES

"inplace/inplace [Vector]"
  forall (f1 :: forall m. Monad m => S.Stream m a -> S.Stream m a)
         (f2 :: forall m. Monad m => S.Stream m a -> S.Stream m a)
         g1 g2 s.
  inplace f1 g1 (inplace f2 g2 s) = inplace (f1 . f2) (g1 . g2) s   #-}


-- | 'Size' hint of a 'Bundle'
size :: Bundle v a -> Size
{-# INLINE size #-}
size = M.size

-- | Attach a 'Size' hint to a 'Bundle'
sized :: Bundle v a -> Size -> Bundle v a
{-# INLINE sized #-}
sized = M.sized

-- Length
-- ------

-- | Length of a 'Bundle'
length :: Bundle v a -> Int
{-# INLINE length #-}
length = unId . M.length

-- | Check if a 'Bundle' is empty
null :: Bundle v a -> Bool
{-# INLINE null #-}
null = unId . M.null

-- Construction
-- ------------

-- | Empty 'Bundle'
empty :: Bundle v a
{-# INLINE empty #-}
empty = M.empty

-- | Singleton 'Bundle'
singleton :: a -> Bundle v a
{-# INLINE singleton #-}
singleton = M.singleton

-- | Replicate a value to a given length
replicate :: Int -> a -> Bundle v a
{-# INLINE replicate #-}
replicate = M.replicate

-- | Generate a stream from its indices
generate :: Int -> (Int -> a) -> Bundle v a
{-# INLINE generate #-}
generate = M.generate

-- | Prepend an element
cons :: a -> Bundle v a -> Bundle v a
{-# INLINE cons #-}
cons = M.cons

-- | Append an element
snoc :: Bundle v a -> a -> Bundle v a
{-# INLINE snoc #-}
snoc = M.snoc

infixr 5 ++
-- | Concatenate two 'Bundle's
(++) :: Bundle v a -> Bundle v a -> Bundle v a
{-# INLINE (++) #-}
(++) = (M.++)

-- Accessing elements
-- ------------------

-- | First element of the 'Bundle' or error if empty
head :: Bundle v a -> a
{-# INLINE head #-}
head = unId . M.head

-- | Last element of the 'Bundle' or error if empty
last :: Bundle v a -> a
{-# INLINE last #-}
last = unId . M.last

infixl 9 !!
-- | Element at the given position
(!!) :: Bundle v a -> Int -> a
{-# INLINE (!!) #-}
s !! i = unId (s M.!! i)

infixl 9 !?
-- | Element at the given position or 'Nothing' if out of bounds
(!?) :: Bundle v a -> Int -> Maybe a
{-# INLINE (!?) #-}
s !? i = unId (s M.!? i)

-- Substreams
-- ----------

-- | Extract a substream of the given length starting at the given position.
slice :: Int   -- ^ starting index
      -> Int   -- ^ length
      -> Bundle v a
      -> Bundle v a
{-# INLINE slice #-}
slice = M.slice

-- | All but the last element
init :: Bundle v a -> Bundle v a
{-# INLINE init #-}
init = M.init

-- | All but the first element
tail :: Bundle v a -> Bundle v a
{-# INLINE tail #-}
tail = M.tail

-- | The first @n@ elements
take :: Int -> Bundle v a -> Bundle v a
{-# INLINE take #-}
take = M.take

-- | All but the first @n@ elements
drop :: Int -> Bundle v a -> Bundle v a
{-# INLINE drop #-}
drop = M.drop

-- Mapping
-- ---------------

-- | Map a function over a 'Bundle'
map :: (a -> b) -> Bundle v a -> Bundle v b
{-# INLINE map #-}
map = M.map

unbox :: Bundle v (Box a) -> Bundle v a
{-# INLINE unbox #-}
unbox = M.unbox

concatMap :: (a -> Bundle v b) -> Bundle v a -> Bundle v b
{-# INLINE concatMap #-}
concatMap = M.concatMap

-- Zipping
-- -------

-- | Pair each element in a 'Bundle' with its index
indexed :: Bundle v a -> Bundle v (Int,a)
{-# INLINE indexed #-}
indexed = M.indexed

-- | Pair each element in a 'Bundle' with its index, starting from the right
-- and counting down
indexedR :: Int -> Bundle v a -> Bundle v (Int,a)
{-# INLINE_FUSED indexedR #-}
indexedR = M.indexedR

-- | Zip two 'Bundle's with the given function
zipWith :: (a -> b -> c) -> Bundle v a -> Bundle v b -> Bundle v c
{-# INLINE zipWith #-}
zipWith = M.zipWith

-- | Zip three 'Bundle's with the given function
zipWith3 :: (a -> b -> c -> d) -> Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
{-# INLINE zipWith3 #-}
zipWith3 = M.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
                    -> Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                    -> Bundle v e
{-# INLINE zipWith4 #-}
zipWith4 = M.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
                    -> Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                    -> Bundle v e -> Bundle v f
{-# INLINE zipWith5 #-}
zipWith5 = M.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
                    -> Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                    -> Bundle v e -> Bundle v f -> Bundle v g
{-# INLINE zipWith6 #-}
zipWith6 = M.zipWith6

zip :: Bundle v a -> Bundle v b -> Bundle v (a,b)
{-# INLINE zip #-}
zip = M.zip

zip3 :: Bundle v a -> Bundle v b -> Bundle v c -> Bundle v (a,b,c)
{-# INLINE zip3 #-}
zip3 = M.zip3

zip4 :: Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                -> Bundle v (a,b,c,d)
{-# INLINE zip4 #-}
zip4 = M.zip4

zip5 :: Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                -> Bundle v e -> Bundle v (a,b,c,d,e)
{-# INLINE zip5 #-}
zip5 = M.zip5

zip6 :: Bundle v a -> Bundle v b -> Bundle v c -> Bundle v d
                -> Bundle v e -> Bundle v f -> Bundle v (a,b,c,d,e,f)
{-# INLINE zip6 #-}
zip6 = M.zip6

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: (a -> Bool) -> Bundle v a -> Bundle v a
{-# INLINE filter #-}
filter = M.filter

-- | Longest prefix of elements that satisfy the predicate
takeWhile :: (a -> Bool) -> Bundle v a -> Bundle v a
{-# INLINE takeWhile #-}
takeWhile = M.takeWhile

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhile :: (a -> Bool) -> Bundle v a -> Bundle v a
{-# INLINE dropWhile #-}
dropWhile = M.dropWhile

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the 'Bundle' contains an element
elem :: Eq a => a -> Bundle v a -> Bool
{-# INLINE elem #-}
elem x = unId . M.elem x

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: Eq a => a -> Bundle v a -> Bool
{-# INLINE notElem #-}
notElem x = unId . M.notElem x

-- | Yield 'Just' the first element matching the predicate or 'Nothing' if no
-- such element exists.
find :: (a -> Bool) -> Bundle v a -> Maybe a
{-# INLINE find #-}
find f = unId . M.find f

-- | Yield 'Just' the index of the first element matching the predicate or
-- 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Bundle v a -> Maybe Int
{-# INLINE findIndex #-}
findIndex f = unId . M.findIndex f

-- Folding
-- -------

-- | Left fold
foldl :: (a -> b -> a) -> a -> Bundle v b -> a
{-# INLINE foldl #-}
foldl f z = unId . M.foldl f z

-- | Left fold on non-empty 'Bundle's
foldl1 :: (a -> a -> a) -> Bundle v a -> a
{-# INLINE foldl1 #-}
foldl1 f = unId . M.foldl1 f

-- | Left fold with strict accumulator
foldl' :: (a -> b -> a) -> a -> Bundle v b -> a
{-# INLINE foldl' #-}
foldl' f z = unId . M.foldl' f z

-- | Left fold on non-empty 'Bundle's with strict accumulator
foldl1' :: (a -> a -> a) -> Bundle v a -> a
{-# INLINE foldl1' #-}
foldl1' f = unId . M.foldl1' f

-- | Right fold
foldr :: (a -> b -> b) -> b -> Bundle v a -> b
{-# INLINE foldr #-}
foldr f z = unId . M.foldr f z

-- | Right fold on non-empty 'Bundle's
foldr1 :: (a -> a -> a) -> Bundle v a -> a
{-# INLINE foldr1 #-}
foldr1 f = unId . M.foldr1 f

-- Specialised folds
-- -----------------

and :: Bundle v Bool -> Bool
{-# INLINE and #-}
and = unId . M.and

or :: Bundle v Bool -> Bool
{-# INLINE or #-}
or = unId . M.or

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: (s -> Maybe (a, s)) -> s -> Bundle v a
{-# INLINE unfoldr #-}
unfoldr = M.unfoldr

-- | Unfold at most @n@ elements
unfoldrN :: Int -> (s -> Maybe (a, s)) -> s -> Bundle v a
{-# INLINE unfoldrN #-}
unfoldrN = M.unfoldrN

-- | Unfold exactly @n@ elements
--
-- @since 0.12.2.0
unfoldrExactN :: Int -> (s -> (a, s)) -> s -> Bundle v a
{-# INLINE unfoldrExactN #-}
unfoldrExactN = M.unfoldrExactN

-- | /O(n)/ Apply function \(\max(n - 1, 0)\) times to an initial value, producing a pure
-- bundle of exact length \(\max(n, 0)\). Zeroth element will contain the initial value.
iterateN :: Int -> (a -> a) -> a -> Bundle v a
{-# INLINE iterateN #-}
iterateN = M.iterateN

-- Scans
-- -----

-- | Prefix scan
prescanl :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE prescanl #-}
prescanl = M.prescanl

-- | Prefix scan with strict accumulator
prescanl' :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE prescanl' #-}
prescanl' = M.prescanl'

-- | Suffix scan
postscanl :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE postscanl #-}
postscanl = M.postscanl

-- | Suffix scan with strict accumulator
postscanl' :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE postscanl' #-}
postscanl' = M.postscanl'

-- | Haskell-style scan
scanl :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE scanl #-}
scanl = M.scanl

-- | Haskell-style scan with strict accumulator
scanl' :: (a -> b -> a) -> a -> Bundle v b -> Bundle v a
{-# INLINE scanl' #-}
scanl' = M.scanl'

-- | Scan over a non-empty 'Bundle'
scanl1 :: (a -> a -> a) -> Bundle v a -> Bundle v a
{-# INLINE scanl1 #-}
scanl1 = M.scanl1

-- | Scan over a non-empty 'Bundle' with a strict accumulator
scanl1' :: (a -> a -> a) -> Bundle v a -> Bundle v a
{-# INLINE scanl1' #-}
scanl1' = M.scanl1'


-- Comparisons
-- -----------

-- | Check if two 'Bundle's are equal
eq :: (Eq a) => Bundle v a -> Bundle v a -> Bool
{-# INLINE eq #-}
eq = eqBy (==)

eqBy :: (a -> b -> Bool) -> Bundle v a -> Bundle v b -> Bool
{-# INLINE eqBy #-}
eqBy e x y = unId (M.eqBy e x y)

-- | Lexicographically compare two 'Bundle's
cmp :: (Ord a) => Bundle v a -> Bundle v a -> Ordering
{-# INLINE cmp #-}
cmp = cmpBy compare

cmpBy :: (a ->  b -> Ordering) -> Bundle v a -> Bundle v b -> Ordering
{-# INLINE cmpBy #-}
cmpBy c x y = unId (M.cmpBy c x y)

instance Eq a => Eq (M.Bundle Id v a) where
  {-# INLINE (==) #-}
  (==) = eq

instance Ord a => Ord (M.Bundle Id v a) where
  {-# INLINE compare #-}
  compare = cmp

#if MIN_VERSION_base(4,9,0)
instance Eq1 (M.Bundle Id v) where
  {-# INLINE liftEq #-}
  liftEq = eqBy

instance Ord1 (M.Bundle Id v) where
  {-# INLINE liftCompare #-}
  liftCompare = cmpBy
#endif

-- Monadic combinators
-- -------------------

-- | Apply a monadic action to each element of the stream, producing a monadic
-- stream of results
mapM :: Monad m => (a -> m b) -> Bundle v a -> M.Bundle m v b
{-# INLINE mapM #-}
mapM f = M.mapM f . lift

-- | Apply a monadic action to each element of the stream
mapM_ :: Monad m => (a -> m b) -> Bundle v a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = M.mapM_ f . lift

zipWithM :: Monad m => (a -> b -> m c) -> Bundle v a -> Bundle v b -> M.Bundle m v c
{-# INLINE zipWithM #-}
zipWithM f as bs = M.zipWithM f (lift as) (lift bs)

zipWithM_ :: Monad m => (a -> b -> m c) -> Bundle v a -> Bundle v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f as bs = M.zipWithM_ f (lift as) (lift bs)

-- | Yield a monadic stream of elements that satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Bundle v a -> M.Bundle m v a
{-# INLINE filterM #-}
filterM f = M.filterM f . lift

-- | /O(n)/ Apply monadic function to each element of a bundle and
-- discard elements returning Nothing.
--
-- @since 0.12.2.0
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Bundle v a -> M.Bundle m v b
{-# INLINE mapMaybeM #-}
mapMaybeM f = M.mapMaybeM f . lift

-- | Monadic fold
foldM :: Monad m => (a -> b -> m a) -> a -> Bundle v b -> m a
{-# INLINE foldM #-}
foldM m z = M.foldM m z . lift

-- | Monadic fold over non-empty stream
fold1M :: Monad m => (a -> a -> m a) -> Bundle v a -> m a
{-# INLINE fold1M #-}
fold1M m = M.fold1M m . lift

-- | Monadic fold with strict accumulator
foldM' :: Monad m => (a -> b -> m a) -> a -> Bundle v b -> m a
{-# INLINE foldM' #-}
foldM' m z = M.foldM' m z . lift

-- | Monad fold over non-empty stream with strict accumulator
fold1M' :: Monad m => (a -> a -> m a) -> Bundle v a -> m a
{-# INLINE fold1M' #-}
fold1M' m = M.fold1M' m . lift

-- Enumerations
-- ------------

-- | Yield a 'Bundle' of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc.
enumFromStepN :: Num a => a -> a -> Int -> Bundle v a
{-# INLINE enumFromStepN #-}
enumFromStepN = M.enumFromStepN

-- | Enumerate values
--
-- /WARNING:/ This operations can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromTo :: Enum a => a -> a -> Bundle v a
{-# INLINE enumFromTo #-}
enumFromTo = M.enumFromTo

-- | Enumerate values with a given step.
--
-- /WARNING:/ This operations is very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Bundle v a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = M.enumFromThenTo

-- Conversions
-- -----------

-- | Convert a 'Bundle' to a list
toList :: Bundle v a -> [a]
{-# INLINE toList #-}
-- toList s = unId (M.toList s)
toList s = build (\c n -> toListFB c n s)

-- This supports foldr/build list fusion that GHC implements
toListFB :: (a -> b -> b) -> b -> Bundle v a -> b
{-# INLINE [0] toListFB #-}
toListFB c n M.Bundle{M.sElems = Stream step t} = go t
  where
    go s = case unId (step s) of
             Yield x s' -> x `c` go s'
             Skip    s' -> go s'
             Done       -> n

-- | Create a 'Bundle' from a list
fromList :: [a] -> Bundle v a
{-# INLINE fromList #-}
fromList = M.fromList

-- | Create a 'Bundle' from the first @n@ elements of a list
--
-- > fromListN n xs = fromList (take n xs)
fromListN :: Int -> [a] -> Bundle v a
{-# INLINE fromListN #-}
fromListN = M.fromListN

unsafeFromList :: Size -> [a] -> Bundle v a
{-# INLINE unsafeFromList #-}
unsafeFromList = M.unsafeFromList

fromVector :: Vector v a => v a -> Bundle v a
{-# INLINE fromVector #-}
fromVector = M.fromVector

reVector :: Bundle u a -> Bundle v a
{-# INLINE reVector #-}
reVector = M.reVector

fromVectors :: Vector v a => [v a] -> Bundle v a
{-# INLINE fromVectors #-}
fromVectors = M.fromVectors

concatVectors :: Vector v a => Bundle u (v a) -> Bundle v a
{-# INLINE concatVectors #-}
concatVectors = M.concatVectors

-- | Create a 'Bundle' of values from a 'Bundle' of streamable things
flatten :: (a -> s) -> (s -> Step s b) -> Size -> Bundle v a -> Bundle v b
{-# INLINE_FUSED flatten #-}
flatten mk istep sz = M.flatten (return . mk) (return . istep) sz . lift

