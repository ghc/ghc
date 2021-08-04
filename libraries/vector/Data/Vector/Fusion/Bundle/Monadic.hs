{-# LANGUAGE CPP, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, Rank2Types, BangPatterns, KindSignatures, GADTs, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Fusion.Bundle.Monadic
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic bundles.
--

module Data.Vector.Fusion.Bundle.Monadic (
  Bundle(..), Chunk(..), lift,

  -- * Size hints
  size, sized,

  -- * Length
  length, null,

  -- * Construction
  empty, singleton, cons, snoc, replicate, replicateM, generate, generateM, (++),

  -- * Accessing elements
  head, last, (!!), (!?),

  -- * Substreams
  slice, init, tail, take, drop,

  -- * Mapping
  map, mapM, mapM_, trans, unbox, concatMap, flatten,

  -- * Zipping
  indexed, indexedR, zipWithM_,
  zipWithM, zipWith3M, zipWith4M, zipWith5M, zipWith6M,
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- * Comparisons
  eqBy, cmpBy,

  -- * Filtering
  filter, filterM, mapMaybeM, takeWhile, takeWhileM, dropWhile, dropWhileM,

  -- * Searching
  elem, notElem, find, findM, findIndex, findIndexM,

  -- * Folding
  foldl, foldlM, foldl1, foldl1M, foldM, fold1M,
  foldl', foldlM', foldl1', foldl1M', foldM', fold1M',
  foldr, foldrM, foldr1, foldr1M,

  -- * Specialised folds
  and, or, concatMapM,

  -- * Unfolding
  unfoldr, unfoldrM,
  unfoldrN, unfoldrNM,
  unfoldrExactN, unfoldrExactNM,
  iterateN, iterateNM,

  -- * Scans
  prescanl, prescanlM, prescanl', prescanlM',
  postscanl, postscanlM, postscanl', postscanlM',
  scanl, scanlM, scanl', scanlM',
  scanl1, scanl1M, scanl1', scanl1M',

  -- * Enumerations
  enumFromStepN, enumFromTo, enumFromThenTo,

  -- * Conversions
  toList, fromList, fromListN, unsafeFromList,
  fromVector, reVector, fromVectors, concatVectors,
  fromStream, chunks, elements
) where

import Data.Vector.Generic.Base
import qualified Data.Vector.Generic.Mutable.Base as M
import Data.Vector.Fusion.Bundle.Size
import Data.Vector.Fusion.Util ( Box(..), delay_inline, Id(..) )
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Control.Monad.Primitive

import qualified Data.List as List
import Data.Char      ( ord )
import GHC.Base       ( unsafeChr )
import Control.Monad  ( liftM )
import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, mapM, mapM_, concatMap,
                        zipWith, zipWith3, zip, zip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

import Data.Int  ( Int8, Int16, Int32 )
import Data.Word ( Word8, Word16, Word32, Word64 )

#if !MIN_VERSION_base(4,8,0)
import Data.Word ( Word )
#endif

#include "vector.h"
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS > 32
import Data.Int  ( Int64 )
#endif

data Chunk v a = Chunk Int (forall m. (PrimMonad m, Vector v a) => Mutable v (PrimState m) a -> m ())

-- | Monadic streams
data Bundle m v a = Bundle { sElems  :: Stream m a
                           , sChunks :: Stream m (Chunk v a)
                           , sVector :: Maybe (v a)
                           , sSize   :: Size
                           }

-- | Convert a pure stream to a monadic stream
lift :: Monad m => Bundle Id v a -> Bundle m v a
{-# INLINE_FUSED lift #-}
lift (Bundle (Stream step s) (Stream vstep t) v sz)
    = Bundle (Stream (return . unId . step) s)
             (Stream (return . unId . vstep) t) v sz

fromStream :: Monad m => Stream m a -> Size -> Bundle m v a
{-# INLINE fromStream #-}
fromStream (Stream step t) sz = Bundle (Stream step t) (Stream step' t) Nothing sz
  where
    step' s = do r <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> M.basicUnsafeWrite v 0 x)) r

chunks :: Bundle m v a -> Stream m (Chunk v a)
{-# INLINE chunks #-}
chunks = sChunks

elements :: Bundle m v a -> Stream m a
{-# INLINE elements #-}
elements = sElems

-- | 'Size' hint of a 'Bundle'
size :: Bundle m v a -> Size
{-# INLINE size #-}
size = sSize

-- | Attach a 'Size' hint to a 'Bundle'
sized :: Bundle m v a -> Size -> Bundle m v a
{-# INLINE_FUSED sized #-}
sized s sz = s { sSize = sz }

-- Length
-- ------

-- | Length of a 'Bundle'
length :: Monad m => Bundle m v a -> m Int
{-# INLINE_FUSED length #-}
length Bundle{sSize = Exact n}  = return n
length Bundle{sChunks = s} = S.foldl' (\n (Chunk k _) -> n+k) 0 s

-- | Check if a 'Bundle' is empty
null :: Monad m => Bundle m v a -> m Bool
{-# INLINE_FUSED null #-}
null Bundle{sSize = Exact n} = return (n == 0)
null Bundle{sChunks = s} = S.foldr (\(Chunk n _) z -> n == 0 && z) True s

-- Construction
-- ------------

-- | Empty 'Bundle'
empty :: Monad m => Bundle m v a
{-# INLINE_FUSED empty #-}
empty = fromStream S.empty (Exact 0)

-- | Singleton 'Bundle'
singleton :: Monad m => a -> Bundle m v a
{-# INLINE_FUSED singleton #-}
singleton x = fromStream (S.singleton x) (Exact 1)

-- | Replicate a value to a given length
replicate :: Monad m => Int -> a -> Bundle m v a
{-# INLINE_FUSED replicate #-}
replicate n x = Bundle (S.replicate n x)
                       (S.singleton $ Chunk len (\v -> M.basicSet v x))
                       Nothing
                       (Exact len)
  where
    len = delay_inline max n 0

-- | Yield a 'Bundle' of values obtained by performing the monadic action the
-- given number of times
replicateM :: Monad m => Int -> m a -> Bundle m v a
{-# INLINE_FUSED replicateM #-}
-- NOTE: We delay inlining max here because GHC will create a join point for
-- the call to newArray# otherwise which is not really nice.
replicateM n p = fromStream (S.replicateM n p) (Exact (delay_inline max n 0))

generate :: Monad m => Int -> (Int -> a) -> Bundle m v a
{-# INLINE generate #-}
generate n f = generateM n (return . f)

-- | Generate a stream from its indices
generateM :: Monad m => Int -> (Int -> m a) -> Bundle m v a
{-# INLINE_FUSED generateM #-}
generateM n f = fromStream (S.generateM n f) (Exact (delay_inline max n 0))

-- | Prepend an element
cons :: Monad m => a -> Bundle m v a -> Bundle m v a
{-# INLINE cons #-}
cons x s = singleton x ++ s

-- | Append an element
snoc :: Monad m => Bundle m v a -> a -> Bundle m v a
{-# INLINE snoc #-}
snoc s x = s ++ singleton x

infixr 5 ++
-- | Concatenate two 'Bundle's
(++) :: Monad m => Bundle m v a -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED (++) #-}
Bundle sa ta _ na ++ Bundle sb tb _ nb = Bundle (sa S.++ sb) (ta S.++ tb) Nothing (na + nb)

-- Accessing elements
-- ------------------

-- | First element of the 'Bundle' or error if empty
head :: Monad m => Bundle m v a -> m a
{-# INLINE_FUSED head #-}
head = S.head . sElems

-- | Last element of the 'Bundle' or error if empty
last :: Monad m => Bundle m v a -> m a
{-# INLINE_FUSED last #-}
last = S.last . sElems

infixl 9 !!
-- | Element at the given position
(!!) :: Monad m => Bundle m v a -> Int -> m a
{-# INLINE (!!) #-}
b !! i = sElems b S.!! i

infixl 9 !?
-- | Element at the given position or 'Nothing' if out of bounds
(!?) :: Monad m => Bundle m v a -> Int -> m (Maybe a)
{-# INLINE (!?) #-}
b !? i = sElems b S.!? i

-- Substreams
-- ----------

-- | Extract a substream of the given length starting at the given position.
slice :: Monad m => Int   -- ^ starting index
                 -> Int   -- ^ length
                 -> Bundle m v a
                 -> Bundle m v a
{-# INLINE slice #-}
slice i n s = take n (drop i s)

-- | All but the last element
init :: Monad m => Bundle m v a -> Bundle m v a
{-# INLINE_FUSED init #-}
init Bundle{sElems = s, sSize = sz} = fromStream (S.init s) (sz-1)

-- | All but the first element
tail :: Monad m => Bundle m v a -> Bundle m v a
{-# INLINE_FUSED tail #-}
tail Bundle{sElems = s, sSize = sz} = fromStream (S.tail s) (sz-1)

-- | The first @n@ elements
take :: Monad m => Int -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED take #-}
take n Bundle{sElems = s, sSize = sz} = fromStream (S.take n s) (smallerThan n sz)

-- | All but the first @n@ elements
drop :: Monad m => Int -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED drop #-}
drop n Bundle{sElems = s, sSize = sz} =
  fromStream (S.drop n s) (clampedSubtract sz (Exact n))

-- Mapping
-- -------

instance Monad m => Functor (Bundle m v) where
  {-# INLINE fmap #-}
  fmap = map
#if MIN_VERSION_base(4,8,0)
  {-# INLINE (<$) #-}
  (<$) = map . const
#endif

-- | Map a function over a 'Bundle'
map :: Monad m => (a -> b) -> Bundle m v a -> Bundle m v b
{-# INLINE map #-}
map f = mapM (return . f)

-- | Map a monadic function over a 'Bundle'
mapM :: Monad m => (a -> m b) -> Bundle m v a -> Bundle m v b
{-# INLINE_FUSED mapM #-}
mapM f Bundle{sElems = s, sSize = n} = fromStream (S.mapM f s) n

-- | Execute a monadic action for each element of the 'Bundle'
mapM_ :: Monad m => (a -> m b) -> Bundle m v a -> m ()
{-# INLINE_FUSED mapM_ #-}
mapM_ m = S.mapM_ m . sElems

-- | Transform a 'Bundle' to use a different monad
trans :: (Monad m, Monad m') => (forall z. m z -> m' z)
                             -> Bundle m v a -> Bundle m' v a
{-# INLINE_FUSED trans #-}
trans f Bundle{sElems = s, sChunks = cs, sVector = v, sSize = n}
  = Bundle { sElems = S.trans f s, sChunks = S.trans f cs, sVector = v, sSize = n }

unbox :: Monad m => Bundle m v (Box a) -> Bundle m v a
{-# INLINE_FUSED unbox #-}
unbox Bundle{sElems = s, sSize = n} = fromStream (S.unbox s) n

-- Zipping
-- -------

-- | Pair each element in a 'Bundle' with its index
indexed :: Monad m => Bundle m v a -> Bundle m v (Int,a)
{-# INLINE_FUSED indexed #-}
indexed Bundle{sElems = s, sSize = n} = fromStream (S.indexed s) n

-- | Pair each element in a 'Bundle' with its index, starting from the right
-- and counting down
indexedR :: Monad m => Int -> Bundle m v a -> Bundle m v (Int,a)
{-# INLINE_FUSED indexedR #-}
indexedR m Bundle{sElems = s, sSize = n} = fromStream (S.indexedR m s) n

-- | Zip two 'Bundle's with the given monadic function
zipWithM :: Monad m => (a -> b -> m c) -> Bundle m v a -> Bundle m v b -> Bundle m v c
{-# INLINE_FUSED zipWithM #-}
zipWithM f Bundle{sElems = sa, sSize = na}
           Bundle{sElems = sb, sSize = nb} = fromStream (S.zipWithM f sa sb) (smaller na nb)

-- FIXME: This might expose an opportunity for inplace execution.
{-# RULES

"zipWithM xs xs [Vector.Bundle]" forall f xs.
  zipWithM f (lift xs) (lift xs) = mapM (\x -> f x x) (lift xs) #-}


zipWithM_ :: Monad m => (a -> b -> m c) -> Bundle m v a -> Bundle m v b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f sa sb = S.zipWithM_ f (sElems sa) (sElems sb)

zipWith3M :: Monad m => (a -> b -> c -> m d) -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
{-# INLINE_FUSED zipWith3M #-}
zipWith3M f Bundle{sElems = sa, sSize = na}
            Bundle{sElems = sb, sSize = nb}
            Bundle{sElems = sc, sSize = nc}
  = fromStream (S.zipWith3M f sa sb sc) (smaller na (smaller nb nc))

zipWith4M :: Monad m => (a -> b -> c -> d -> m e)
                     -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                     -> Bundle m v e
{-# INLINE zipWith4M #-}
zipWith4M f sa sb sc sd
  = zipWithM (\(a,b) (c,d) -> f a b c d) (zip sa sb) (zip sc sd)

zipWith5M :: Monad m => (a -> b -> c -> d -> e -> m f)
                     -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                     -> Bundle m v e -> Bundle m v f
{-# INLINE zipWith5M #-}
zipWith5M f sa sb sc sd se
  = zipWithM (\(a,b,c) (d,e) -> f a b c d e) (zip3 sa sb sc) (zip sd se)

zipWith6M :: Monad m => (a -> b -> c -> d -> e -> f -> m g)
                     -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                     -> Bundle m v e -> Bundle m v f -> Bundle m v g
{-# INLINE zipWith6M #-}
zipWith6M fn sa sb sc sd se sf
  = zipWithM (\(a,b,c) (d,e,f) -> fn a b c d e f) (zip3 sa sb sc)
                                                  (zip3 sd se sf)

zipWith :: Monad m => (a -> b -> c) -> Bundle m v a -> Bundle m v b -> Bundle m v c
{-# INLINE zipWith #-}
zipWith f = zipWithM (\a b -> return (f a b))

zipWith3 :: Monad m => (a -> b -> c -> d)
                    -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
{-# INLINE zipWith3 #-}
zipWith3 f = zipWith3M (\a b c -> return (f a b c))

zipWith4 :: Monad m => (a -> b -> c -> d -> e)
                    -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                    -> Bundle m v e
{-# INLINE zipWith4 #-}
zipWith4 f = zipWith4M (\a b c d -> return (f a b c d))

zipWith5 :: Monad m => (a -> b -> c -> d -> e -> f)
                    -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                    -> Bundle m v e -> Bundle m v f
{-# INLINE zipWith5 #-}
zipWith5 f = zipWith5M (\a b c d e -> return (f a b c d e))

zipWith6 :: Monad m => (a -> b -> c -> d -> e -> f -> g)
                    -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                    -> Bundle m v e -> Bundle m v f -> Bundle m v g
{-# INLINE zipWith6 #-}
zipWith6 fn = zipWith6M (\a b c d e f -> return (fn a b c d e f))

zip :: Monad m => Bundle m v a -> Bundle m v b -> Bundle m v (a,b)
{-# INLINE zip #-}
zip = zipWith (,)

zip3 :: Monad m => Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v (a,b,c)
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 :: Monad m => Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                -> Bundle m v (a,b,c,d)
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 :: Monad m => Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                -> Bundle m v e -> Bundle m v (a,b,c,d,e)
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 :: Monad m => Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d
                -> Bundle m v e -> Bundle m v f -> Bundle m v (a,b,c,d,e,f)
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

-- Comparisons
-- -----------

-- | Check if two 'Bundle's are equal
eqBy :: (Monad m) => (a -> b -> Bool) -> Bundle m v a -> Bundle m v b -> m Bool
{-# INLINE_FUSED eqBy #-}
eqBy eq x y
  | sizesAreDifferent (sSize x) (sSize y) = return False
  | otherwise                             = S.eqBy eq (sElems x) (sElems y)
  where
    sizesAreDifferent :: Size -> Size -> Bool
    sizesAreDifferent (Exact a) (Exact b) = a /= b
    sizesAreDifferent (Exact a) (Max b)   = a > b
    sizesAreDifferent (Max a)   (Exact b) = a < b
    sizesAreDifferent _         _         = False

-- | Lexicographically compare two 'Bundle's
cmpBy :: (Monad m) => (a -> b -> Ordering) -> Bundle m v a -> Bundle m v b -> m Ordering
{-# INLINE_FUSED cmpBy #-}
cmpBy cmp x y = S.cmpBy cmp (sElems x) (sElems y)

-- Filtering
-- ---------

-- | Drop elements which do not satisfy the predicate
filter :: Monad m => (a -> Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE filter #-}
filter f = filterM (return . f)

-- | Drop elements which do not satisfy the monadic predicate
filterM :: Monad m => (a -> m Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED filterM #-}
filterM f Bundle{sElems = s, sSize = n} = fromStream (S.filterM f s) (toMax n)

-- | Apply monadic function to each element and drop all Nothings
--
-- @since 0.12.2.0
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Bundle m v a -> Bundle m v b
{-# INLINE_FUSED mapMaybeM #-}
mapMaybeM f Bundle{sElems = s, sSize = n} = fromStream (S.mapMaybeM f s) (toMax n)

-- | Longest prefix of elements that satisfy the predicate
takeWhile :: Monad m => (a -> Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE takeWhile #-}
takeWhile f = takeWhileM (return . f)

-- | Longest prefix of elements that satisfy the monadic predicate
takeWhileM :: Monad m => (a -> m Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED takeWhileM #-}
takeWhileM f Bundle{sElems = s, sSize = n} = fromStream (S.takeWhileM f s) (toMax n)

-- | Drop the longest prefix of elements that satisfy the predicate
dropWhile :: Monad m => (a -> Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE dropWhile #-}
dropWhile f = dropWhileM (return . f)

-- | Drop the longest prefix of elements that satisfy the monadic predicate
dropWhileM :: Monad m => (a -> m Bool) -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED dropWhileM #-}
dropWhileM f Bundle{sElems = s, sSize = n} = fromStream (S.dropWhileM f s) (toMax n)

-- Searching
-- ---------

infix 4 `elem`
-- | Check whether the 'Bundle' contains an element
elem :: (Monad m, Eq a) => a -> Bundle m v a -> m Bool
{-# INLINE_FUSED elem #-}
elem x = S.elem x . sElems

infix 4 `notElem`
-- | Inverse of `elem`
notElem :: (Monad m, Eq a) => a -> Bundle m v a -> m Bool
{-# INLINE notElem #-}
notElem x = S.notElem x . sElems

-- | Yield 'Just' the first element that satisfies the predicate or 'Nothing'
-- if no such element exists.
find :: Monad m => (a -> Bool) -> Bundle m v a -> m (Maybe a)
{-# INLINE find #-}
find f = findM (return . f)

-- | Yield 'Just' the first element that satisfies the monadic predicate or
-- 'Nothing' if no such element exists.
findM :: Monad m => (a -> m Bool) -> Bundle m v a -> m (Maybe a)
{-# INLINE_FUSED findM #-}
findM f = S.findM f . sElems

-- | Yield 'Just' the index of the first element that satisfies the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Monad m => (a -> Bool) -> Bundle m v a -> m (Maybe Int)
{-# INLINE_FUSED findIndex #-}
findIndex f = findIndexM (return . f)

-- | Yield 'Just' the index of the first element that satisfies the monadic
-- predicate or 'Nothing' if no such element exists.
findIndexM :: Monad m => (a -> m Bool) -> Bundle m v a -> m (Maybe Int)
{-# INLINE_FUSED findIndexM #-}
findIndexM f = S.findIndexM f . sElems

-- Folding
-- -------

-- | Left fold
foldl :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> m a
{-# INLINE foldl #-}
foldl f = foldlM (\a b -> return (f a b))

-- | Left fold with a monadic operator
foldlM :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> m a
{-# INLINE_FUSED foldlM #-}
foldlM m z = S.foldlM m z . sElems

-- | Same as 'foldlM'
foldM :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> m a
{-# INLINE foldM #-}
foldM = foldlM

-- | Left fold over a non-empty 'Bundle'
foldl1 :: Monad m => (a -> a -> a) -> Bundle m v a -> m a
{-# INLINE foldl1 #-}
foldl1 f = foldl1M (\a b -> return (f a b))

-- | Left fold over a non-empty 'Bundle' with a monadic operator
foldl1M :: Monad m => (a -> a -> m a) -> Bundle m v a -> m a
{-# INLINE_FUSED foldl1M #-}
foldl1M f = S.foldl1M f . sElems

-- | Same as 'foldl1M'
fold1M :: Monad m => (a -> a -> m a) -> Bundle m v a -> m a
{-# INLINE fold1M #-}
fold1M = foldl1M

-- | Left fold with a strict accumulator
foldl' :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> m a
{-# INLINE foldl' #-}
foldl' f = foldlM' (\a b -> return (f a b))

-- | Left fold with a strict accumulator and a monadic operator
foldlM' :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> m a
{-# INLINE_FUSED foldlM' #-}
foldlM' m z = S.foldlM' m z . sElems

-- | Same as 'foldlM''
foldM' :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> m a
{-# INLINE foldM' #-}
foldM' = foldlM'

-- | Left fold over a non-empty 'Bundle' with a strict accumulator
foldl1' :: Monad m => (a -> a -> a) -> Bundle m v a -> m a
{-# INLINE foldl1' #-}
foldl1' f = foldl1M' (\a b -> return (f a b))

-- | Left fold over a non-empty 'Bundle' with a strict accumulator and a
-- monadic operator
foldl1M' :: Monad m => (a -> a -> m a) -> Bundle m v a -> m a
{-# INLINE_FUSED foldl1M' #-}
foldl1M' f = S.foldl1M' f . sElems

-- | Same as 'foldl1M''
fold1M' :: Monad m => (a -> a -> m a) -> Bundle m v a -> m a
{-# INLINE fold1M' #-}
fold1M' = foldl1M'

-- | Right fold
foldr :: Monad m => (a -> b -> b) -> b -> Bundle m v a -> m b
{-# INLINE foldr #-}
foldr f = foldrM (\a b -> return (f a b))

-- | Right fold with a monadic operator
foldrM :: Monad m => (a -> b -> m b) -> b -> Bundle m v a -> m b
{-# INLINE_FUSED foldrM #-}
foldrM f z = S.foldrM f z . sElems

-- | Right fold over a non-empty stream
foldr1 :: Monad m => (a -> a -> a) -> Bundle m v a -> m a
{-# INLINE foldr1 #-}
foldr1 f = foldr1M (\a b -> return (f a b))

-- | Right fold over a non-empty stream with a monadic operator
foldr1M :: Monad m => (a -> a -> m a) -> Bundle m v a -> m a
{-# INLINE_FUSED foldr1M #-}
foldr1M f = S.foldr1M f . sElems

-- Specialised folds
-- -----------------

and :: Monad m => Bundle m v Bool -> m Bool
{-# INLINE_FUSED and #-}
and = S.and . sElems

or :: Monad m => Bundle m v Bool -> m Bool
{-# INLINE_FUSED or #-}
or = S.or . sElems

concatMap :: Monad m => (a -> Bundle m v b) -> Bundle m v a -> Bundle m v b
{-# INLINE concatMap #-}
concatMap f = concatMapM (return . f)

concatMapM :: Monad m => (a -> m (Bundle m v b)) -> Bundle m v a -> Bundle m v b
{-# INLINE_FUSED concatMapM #-}
concatMapM f Bundle{sElems = s} = fromStream (S.concatMapM (liftM sElems . f) s) Unknown

-- | Create a 'Bundle' of values from a 'Bundle' of streamable things
flatten :: Monad m => (a -> m s) -> (s -> m (Step s b)) -> Size
                   -> Bundle m v a -> Bundle m v b
{-# INLINE_FUSED flatten #-}
flatten mk istep sz Bundle{sElems = s} = fromStream (S.flatten mk istep s) sz

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldr #-}
unfoldr f = unfoldrM (return . f)

-- | Unfold with a monadic function
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldrM #-}
unfoldrM f s = fromStream (S.unfoldrM f s) Unknown

-- | Unfold at most @n@ elements
unfoldrN :: Monad m => Int -> (s -> Maybe (a, s)) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldrN #-}
unfoldrN n f = unfoldrNM n (return . f)

-- | Unfold at most @n@ elements with a monadic function.
unfoldrNM :: Monad m => Int -> (s -> m (Maybe (a, s))) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldrNM #-}
unfoldrNM n f s = fromStream (S.unfoldrNM n f s) (Max (delay_inline max n 0))

-- | Unfold exactly @n@ elements
--
-- @since 0.12.2.0
unfoldrExactN :: Monad m => Int -> (s -> (a, s)) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldrExactN #-}
unfoldrExactN n f = unfoldrExactNM n (return . f)

-- | Unfold exactly @n@ elements with a monadic function.
--
-- @since 0.12.2.0
unfoldrExactNM :: Monad m => Int -> (s -> m (a, s)) -> s -> Bundle m u a
{-# INLINE_FUSED unfoldrExactNM #-}
unfoldrExactNM n f s = fromStream (S.unfoldrExactNM n f s) (Max (delay_inline max n 0))

-- | /O(n)/ Apply monadic function \(\max(n - 1, 0)\) times to an initial value, producing
-- a monadic bundle of exact length \(\max(n, 0)\). Zeroth element will contain the initial
-- value.
iterateNM :: Monad m => Int -> (a -> m a) -> a -> Bundle m u a
{-# INLINE_FUSED iterateNM #-}
iterateNM n f x0 = fromStream (S.iterateNM n f x0) (Exact (delay_inline max n 0))

-- | /O(n)/ Apply function \(\max(n - 1, 0)\) times to an initial value, producing a
-- monadic bundle of exact length \(\max(n, 0)\). Zeroth element will contain the initial
-- value.
iterateN :: Monad m => Int -> (a -> a) -> a -> Bundle m u a
{-# INLINE_FUSED iterateN #-}
iterateN n f x0 = iterateNM n (return . f) x0

-- Scans
-- -----

-- | Prefix scan
prescanl :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE prescanl #-}
prescanl f = prescanlM (\a b -> return (f a b))

-- | Prefix scan with a monadic operator
prescanlM :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE_FUSED prescanlM #-}
prescanlM f z Bundle{sElems = s, sSize = sz} = fromStream (S.prescanlM f z s) sz

-- | Prefix scan with strict accumulator
prescanl' :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE prescanl' #-}
prescanl' f = prescanlM' (\a b -> return (f a b))

-- | Prefix scan with strict accumulator and a monadic operator
prescanlM' :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE_FUSED prescanlM' #-}
prescanlM' f z Bundle{sElems = s, sSize = sz} = fromStream (S.prescanlM' f z s) sz

-- | Suffix scan
postscanl :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE postscanl #-}
postscanl f = postscanlM (\a b -> return (f a b))

-- | Suffix scan with a monadic operator
postscanlM :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE_FUSED postscanlM #-}
postscanlM f z Bundle{sElems = s, sSize = sz} = fromStream (S.postscanlM f z s) sz

-- | Suffix scan with strict accumulator
postscanl' :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE postscanl' #-}
postscanl' f = postscanlM' (\a b -> return (f a b))

-- | Suffix scan with strict acccumulator and a monadic operator
postscanlM' :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE_FUSED postscanlM' #-}
postscanlM' f z Bundle{sElems = s, sSize = sz} = fromStream (S.postscanlM' f z s) sz

-- | Haskell-style scan
scanl :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE scanl #-}
scanl f = scanlM (\a b -> return (f a b))

-- | Haskell-style scan with a monadic operator
scanlM :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE scanlM #-}
scanlM f z s = z `cons` postscanlM f z s

-- | Haskell-style scan with strict accumulator
scanl' :: Monad m => (a -> b -> a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE scanl' #-}
scanl' f = scanlM' (\a b -> return (f a b))

-- | Haskell-style scan with strict accumulator and a monadic operator
scanlM' :: Monad m => (a -> b -> m a) -> a -> Bundle m v b -> Bundle m v a
{-# INLINE scanlM' #-}
scanlM' f z s = z `seq` (z `cons` postscanlM f z s)

-- | Scan over a non-empty 'Bundle'
scanl1 :: Monad m => (a -> a -> a) -> Bundle m v a -> Bundle m v a
{-# INLINE scanl1 #-}
scanl1 f = scanl1M (\x y -> return (f x y))

-- | Scan over a non-empty 'Bundle' with a monadic operator
scanl1M :: Monad m => (a -> a -> m a) -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED scanl1M #-}
scanl1M f Bundle{sElems = s, sSize = sz} = fromStream (S.scanl1M f s) sz

-- | Scan over a non-empty 'Bundle' with a strict accumulator
scanl1' :: Monad m => (a -> a -> a) -> Bundle m v a -> Bundle m v a
{-# INLINE scanl1' #-}
scanl1' f = scanl1M' (\x y -> return (f x y))

-- | Scan over a non-empty 'Bundle' with a strict accumulator and a monadic
-- operator
scanl1M' :: Monad m => (a -> a -> m a) -> Bundle m v a -> Bundle m v a
{-# INLINE_FUSED scanl1M' #-}
scanl1M' f Bundle{sElems = s, sSize = sz} = fromStream (S.scanl1M' f s) sz

-- Enumerations
-- ------------

-- The Enum class is broken for this, there just doesn't seem to be a
-- way to implement this generically. We have to specialise for as many types
-- as we can but this doesn't help in polymorphic loops.

-- | Yield a 'Bundle' of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc.
enumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Bundle m v a
{-# INLINE_FUSED enumFromStepN #-}
enumFromStepN x y n = fromStream (S.enumFromStepN x y n) (Exact (delay_inline max n 0))

-- | Enumerate values
--
-- /WARNING:/ This operation can be very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromTo :: (Enum a, Monad m) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo #-}
enumFromTo x y = fromList [x .. y]

-- NOTE: We use (x+1) instead of (succ x) below because the latter checks for
-- overflow which can't happen here.

-- FIXME: add "too large" test for Int
enumFromTo_small :: (Integral a, Monad m) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo_small #-}
enumFromTo_small x y = x `seq` y `seq` fromStream (Stream step (Just x)) (Exact n)
  where
    n = delay_inline max (fromIntegral y - fromIntegral x + 1) 0

    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done

{-# RULES

"enumFromTo<Int8> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Int8 -> Int8 -> Bundle m v Int8

"enumFromTo<Int16> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Int16 -> Int16 -> Bundle m v Int16

"enumFromTo<Word8> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Word8 -> Word8 -> Bundle m v Word8

"enumFromTo<Word16> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Word16 -> Word16 -> Bundle m v Word16   #-}



#if WORD_SIZE_IN_BITS > 32

{-# RULES

"enumFromTo<Int32> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Int32 -> Int32 -> Bundle m v Int32

"enumFromTo<Word32> [Bundle]"
  enumFromTo = enumFromTo_small :: Monad m => Word32 -> Word32 -> Bundle m v Word32   #-}

#endif

-- NOTE: We could implement a generic "too large" test:
--
-- len x y | x > y = 0
--         | n > 0 && n <= fromIntegral (maxBound :: Int) = fromIntegral n
--         | otherwise = error
--   where
--     n = y-x+1
--
-- Alas, GHC won't eliminate unnecessary comparisons (such as n >= 0 for
-- unsigned types). See http://hackage.haskell.org/trac/ghc/ticket/3744
--

enumFromTo_int :: forall m v. Monad m => Int -> Int -> Bundle m v Int
{-# INLINE_FUSED enumFromTo_int #-}
enumFromTo_int x y = x `seq` y `seq` fromStream (Stream step (Just x)) (Exact (len x y))
  where
    {-# INLINE [0] len #-}
    len :: Int -> Int -> Int
    len u v | u > v     = 0
            | otherwise = BOUNDS_CHECK(check) "enumFromTo" "vector too large"
                          (n > 0)
                        $ n
      where
        n = v-u+1

    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done

enumFromTo_intlike :: (Integral a, Monad m) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo_intlike #-}
enumFromTo_intlike x y = x `seq` y `seq` fromStream (Stream step (Just x)) (Exact (len x y))
  where
    {-# INLINE [0] len #-}
    len u v | u > v     = 0
            | otherwise = BOUNDS_CHECK(check) "enumFromTo" "vector too large"
                          (n > 0)
                        $ fromIntegral n
      where
        n = v-u+1

    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done

{-# RULES

"enumFromTo<Int> [Bundle]"
  enumFromTo = enumFromTo_int :: Monad m => Int -> Int -> Bundle m v Int

#if WORD_SIZE_IN_BITS > 32

"enumFromTo<Int64> [Bundle]"
  enumFromTo = enumFromTo_intlike :: Monad m => Int64 -> Int64 -> Bundle m v Int64    #-}

#else

"enumFromTo<Int32> [Bundle]"
  enumFromTo = enumFromTo_intlike :: Monad m => Int32 -> Int32 -> Bundle m v Int32    #-}

#endif



enumFromTo_big_word :: (Integral a, Monad m) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo_big_word #-}
enumFromTo_big_word x y = x `seq` y `seq` fromStream (Stream step (Just x)) (Exact (len x y))
  where
    {-# INLINE [0] len #-}
    len u v | u > v     = 0
            | otherwise = BOUNDS_CHECK(check) "enumFromTo" "vector too large"
                          (n < fromIntegral (maxBound :: Int))
                        $ fromIntegral (n+1)
      where
        n = v-u

    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done

{-# RULES

"enumFromTo<Word> [Bundle]"
  enumFromTo = enumFromTo_big_word :: Monad m => Word -> Word -> Bundle m v Word

"enumFromTo<Word64> [Bundle]"
  enumFromTo = enumFromTo_big_word
                        :: Monad m => Word64 -> Word64 -> Bundle m v Word64

#if WORD_SIZE_IN_BITS == 32

"enumFromTo<Word32> [Bundle]"
  enumFromTo = enumFromTo_big_word
                        :: Monad m => Word32 -> Word32 -> Bundle m v Word32

#endif

"enumFromTo<Integer> [Bundle]"
  enumFromTo = enumFromTo_big_word
                        :: Monad m => Integer -> Integer -> Bundle m v Integer   #-}


#if WORD_SIZE_IN_BITS > 32

-- FIXME: the "too large" test is totally wrong
enumFromTo_big_int :: (Integral a, Monad m) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo_big_int #-}
enumFromTo_big_int x y = x `seq` y `seq` fromStream (Stream step (Just x)) (Exact (len x y))
  where
    {-# INLINE [0] len #-}
    len u v | u > v     = 0
            | otherwise = BOUNDS_CHECK(check) "enumFromTo" "vector too large"
                          (n > 0 && n <= fromIntegral (maxBound :: Int))
                        $ fromIntegral n
      where
        n = v-u+1

    {-# INLINE_INNER step #-}
    step Nothing              = return $ Done
    step (Just z) | z == y    = return $ Yield z Nothing
                  | z <  y    = return $ Yield z (Just (z+1))
                  | otherwise = return $ Done


{-# RULES

"enumFromTo<Int64> [Bundle]"
  enumFromTo = enumFromTo_big_int :: Monad m => Int64 -> Int64 -> Bundle m v Int64   #-}



#endif

enumFromTo_char :: Monad m => Char -> Char -> Bundle m v Char
{-# INLINE_FUSED enumFromTo_char #-}
enumFromTo_char x y = x `seq` y `seq` fromStream (Stream step xn) (Exact n)
  where
    xn = ord x
    yn = ord y

    n = delay_inline max 0 (yn - xn + 1)

    {-# INLINE_INNER step #-}
    step zn | zn <= yn  = return $ Yield (unsafeChr zn) (zn+1)
            | otherwise = return $ Done

{-# RULES

"enumFromTo<Char> [Bundle]"
  enumFromTo = enumFromTo_char   #-}



------------------------------------------------------------------------

-- Specialise enumFromTo for Float and Double.
-- Also, try to do something about pairs?

enumFromTo_double :: (Monad m, Ord a, RealFrac a) => a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromTo_double #-}
enumFromTo_double n m = n `seq` m `seq` fromStream (Stream step ini) (Max (len n lim))
  where
    lim = m + 1/2 -- important to float out

    {-# INLINE [0] len #-}
    len x y | x > y     = 0
            | otherwise = BOUNDS_CHECK(check) "enumFromTo" "vector too large"
                          (l > 0)
                        $ fromIntegral l
      where
        l :: Integer
        l = truncate (y-x)+2

    {-# INLINE_INNER step #-}
-- GHC changed definition of Enum for Double in GHC8.6 so we have to
-- accomodate both definitions in order to preserve validity of
-- rewrite rule
--
--  ISSUE:  https://gitlab.haskell.org/ghc/ghc/issues/15081
--  COMMIT: https://gitlab.haskell.org/ghc/ghc/commit/4ffaf4b67773af4c72d92bb8b6c87b1a7d34ac0f
#if MIN_VERSION_base(4,12,0)
    ini = 0
    step x | x' <= lim = return $ Yield x' (x+1)
           | otherwise = return $ Done
           where
             x' = x + n
#else
    ini = n
    step x | x <= lim  = return $ Yield x (x+1)
           | otherwise = return $ Done
#endif

{-# RULES

"enumFromTo<Double> [Bundle]"
  enumFromTo = enumFromTo_double :: Monad m => Double -> Double -> Bundle m v Double

"enumFromTo<Float> [Bundle]"
  enumFromTo = enumFromTo_double :: Monad m => Float -> Float -> Bundle m v Float   #-}



------------------------------------------------------------------------

-- | Enumerate values with a given step.
--
-- /WARNING:/ This operation is very inefficient. If at all possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: (Enum a, Monad m) => a -> a -> a -> Bundle m v a
{-# INLINE_FUSED enumFromThenTo #-}
enumFromThenTo x y z = fromList [x, y .. z]

-- FIXME: Specialise enumFromThenTo.

-- Conversions
-- -----------

-- | Convert a 'Bundle' to a list
toList :: Monad m => Bundle m v a -> m [a]
{-# INLINE toList #-}
toList = foldr (:) []

-- | Convert a list to a 'Bundle'
fromList :: Monad m => [a] -> Bundle m v a
{-# INLINE fromList #-}
fromList xs = unsafeFromList Unknown xs

-- | Convert the first @n@ elements of a list to a 'Bundle'
fromListN :: Monad m => Int -> [a] -> Bundle m v a
{-# INLINE_FUSED fromListN #-}
fromListN n xs = fromStream (S.fromListN n xs) (Max (delay_inline max n 0))

-- | Convert a list to a 'Bundle' with the given 'Size' hint.
unsafeFromList :: Monad m => Size -> [a] -> Bundle m v a
{-# INLINE_FUSED unsafeFromList #-}
unsafeFromList sz xs = fromStream (S.fromList xs) sz

fromVector :: (Monad m, Vector v a) => v a -> Bundle m v a
{-# INLINE_FUSED fromVector #-}
fromVector v = v `seq` n `seq` Bundle (Stream step 0)
                                      (Stream vstep True)
                                      (Just v)
                                      (Exact n)
  where
    n = basicLength v

    {-# INLINE step #-}
    step i | i >= n = return Done
           | otherwise = case basicUnsafeIndexM v i of
                           Box x -> return $ Yield x (i+1)


    {-# INLINE vstep #-}
    vstep True  = return (Yield (Chunk (basicLength v) (\mv -> basicUnsafeCopy mv v)) False)
    vstep False = return Done

fromVectors :: forall m v a. (Monad m, Vector v a) => [v a] -> Bundle m v a
{-# INLINE_FUSED fromVectors #-}
fromVectors us = Bundle (Stream pstep (Left us))
                        (Stream vstep us)
                        Nothing
                        (Exact n)
  where
    n = List.foldl' (\k v -> k + basicLength v) 0 us

    pstep (Left []) = return Done
    pstep (Left (v:vs)) = basicLength v `seq` return (Skip (Right (v,0,vs)))

    pstep (Right (v,i,vs))
      | i >= basicLength v = return $ Skip (Left vs)
      | otherwise          = case basicUnsafeIndexM v i of
                               Box x -> return $ Yield x (Right (v,i+1,vs))

    -- FIXME: work around bug in GHC 7.6.1
    vstep :: [v a] -> m (Step [v a] (Chunk v a))
    vstep [] = return Done
    vstep (v:vs) = return $ Yield (Chunk (basicLength v)
                                         (\mv -> INTERNAL_CHECK(check) "concatVectors" "length mismatch"
                                                                       (M.basicLength mv == basicLength v)
                                                 $ basicUnsafeCopy mv v)) vs


concatVectors :: (Monad m, Vector v a) => Bundle m u (v a) -> Bundle m v a
{-# INLINE_FUSED concatVectors #-}
concatVectors Bundle{sElems = Stream step t}
  = Bundle (Stream pstep (Left t))
           (Stream vstep t)
           Nothing
           Unknown
  where
    pstep (Left s) = do
      r <- step s
      case r of
        Yield v s' -> basicLength v `seq` return (Skip (Right (v,0,s')))
        Skip    s' -> return (Skip (Left s'))
        Done       -> return Done

    pstep (Right (v,i,s))
      | i >= basicLength v = return (Skip (Left s))
      | otherwise          = case basicUnsafeIndexM v i of
                               Box x -> return (Yield x (Right (v,i+1,s)))


    vstep s = do
      r <- step s
      case r of
        Yield v s' -> return (Yield (Chunk (basicLength v)
                                           (\mv -> INTERNAL_CHECK(check) "concatVectors" "length mismatch"
                                                                          (M.basicLength mv == basicLength v)
                                                   $ basicUnsafeCopy mv v)) s')
        Skip    s' -> return (Skip s')
        Done       -> return Done

reVector :: Monad m => Bundle m u a -> Bundle m v a
{-# INLINE_FUSED reVector #-}
reVector Bundle{sElems = s, sSize = n} = fromStream s n

{-# RULES

"reVector [Vector]"
  reVector = id

"reVector/reVector [Vector]" forall s.
  reVector (reVector s) = s   #-}



