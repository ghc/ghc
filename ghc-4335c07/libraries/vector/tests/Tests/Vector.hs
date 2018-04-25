module Tests.Vector (tests) where

import Boilerplater
import Utilities as Util

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Primitive
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Fusion.Bundle as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Show.Functions ()
import Data.List
import Data.Monoid
import qualified Control.Applicative as Applicative
import System.Random       (Random)

import Data.Functor.Identity
import Control.Monad.Trans.Writer

#define COMMON_CONTEXT(a, v) \
 VANILLA_CONTEXT(a, v), VECTOR_CONTEXT(a, v)

#define VANILLA_CONTEXT(a, v) \
  Eq a,     Show a,     Arbitrary a,     CoArbitrary a,     TestData a,     Model a ~ a,        EqTest a ~ Property

#define VECTOR_CONTEXT(a, v) \
  Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a), TestData (v a), Model (v a) ~ [a],  EqTest (v a) ~ Property, V.Vector v a

-- TODO: implement Vector equivalents of list functions for some of the commented out properties

-- TODO: test and implement some of these other Prelude functions:
--  mapM *
--  mapM_ *
--  sequence
--  sequence_
--  sum *
--  product *
--  scanl *
--  scanl1 *
--  scanr *
--  scanr1 *
--  lookup *
--  lines
--  words
--  unlines
--  unwords
-- NB: this is an exhaustive list of all Prelude list functions that make sense for vectors.
-- Ones with *s are the most plausible candidates.

-- TODO: add tests for the other extra functions
-- IVector exports still needing tests:
--  copy,
--  slice,
--  (//), update, bpermute,
--  prescanl, prescanl',
--  new,
--  unsafeSlice, unsafeIndex,
--  vlength, vnew

-- TODO: test non-IVector stuff?

testSanity :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testSanity _ = [
        testProperty "fromList.toList == id" prop_fromList_toList,
        testProperty "toList.fromList == id" prop_toList_fromList,
        testProperty "unstream.stream == id" prop_unstream_stream,
        testProperty "stream.unstream == id" prop_stream_unstream
    ]
  where
    prop_fromList_toList (v :: v a)        = (V.fromList . V.toList)                        v == v
    prop_toList_fromList (l :: [a])        = ((V.toList :: v a -> [a]) . V.fromList)        l == l
    prop_unstream_stream (v :: v a)        = (V.unstream . V.stream)                        v == v
    prop_stream_unstream (s :: S.Bundle v a) = ((V.stream :: v a -> S.Bundle v a) . V.unstream) s == s

testPolymorphicFunctions :: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT(Int, v)) => v a -> [Test]
testPolymorphicFunctions _ = $(testProperties [
        'prop_eq,

        -- Length information
        'prop_length, 'prop_null,

        -- Indexing (FIXME)
        'prop_index, 'prop_safeIndex, 'prop_head, 'prop_last,
        'prop_unsafeIndex, 'prop_unsafeHead, 'prop_unsafeLast,

        -- Monadic indexing (FIXME)
        {- 'prop_indexM, 'prop_headM, 'prop_lastM,
        'prop_unsafeIndexM, 'prop_unsafeHeadM, 'prop_unsafeLastM, -}

        -- Subvectors (FIXME)
        'prop_slice, 'prop_init, 'prop_tail, 'prop_take, 'prop_drop,
        'prop_splitAt,
        {- 'prop_unsafeSlice, 'prop_unsafeInit, 'prop_unsafeTail,
        'prop_unsafeTake, 'prop_unsafeDrop, -}

        -- Initialisation (FIXME)
        'prop_empty, 'prop_singleton, 'prop_replicate,
        'prop_generate, 'prop_iterateN,

        -- Monadic initialisation (FIXME)
        {- 'prop_replicateM, 'prop_generateM, 'prop_create, -}

        -- Unfolding (FIXME)
        {- 'prop_unfoldr, prop_unfoldrN, -}
        'prop_constructN, 'prop_constructrN,

        -- Enumeration? (FIXME?)

        -- Concatenation (FIXME)
        'prop_cons, 'prop_snoc, 'prop_append,
        'prop_concat,

        -- Restricting memory usage
        'prop_force,


        -- Bulk updates (FIXME)
        'prop_upd,
        {- 'prop_update, 'prop_update_,
        'prop_unsafeUpd, 'prop_unsafeUpdate, 'prop_unsafeUpdate_, -}

        -- Accumulations (FIXME)
        'prop_accum,
        {- 'prop_accumulate, 'prop_accumulate_,
        'prop_unsafeAccum, 'prop_unsafeAccumulate, 'prop_unsafeAccumulate_, -}

        -- Permutations
        'prop_reverse, 'prop_backpermute,
        {- 'prop_unsafeBackpermute, -}

        -- Elementwise indexing
        {- 'prop_indexed, -}

        -- Mapping
        'prop_map, 'prop_imap, 'prop_concatMap,

        -- Monadic mapping
        {- 'prop_mapM, 'prop_mapM_, 'prop_forM, 'prop_forM_, -}
        'prop_imapM, 'prop_imapM_,

        -- Zipping
        'prop_zipWith, 'prop_zipWith3, {- ... -}
        'prop_izipWith, 'prop_izipWith3, {- ... -}
        'prop_izipWithM, 'prop_izipWithM_,
        {- 'prop_zip, ... -}

        -- Monadic zipping
        {- 'prop_zipWithM, 'prop_zipWithM_, -}

        -- Unzipping
        {- 'prop_unzip, ... -}

        -- Filtering
        'prop_filter, 'prop_ifilter, {- prop_filterM, -}
        'prop_takeWhile, 'prop_dropWhile,

        -- Paritioning
        'prop_partition, {- 'prop_unstablePartition, -}
        'prop_span, 'prop_break,

        -- Searching
        'prop_elem, 'prop_notElem,
        'prop_find, 'prop_findIndex, 'prop_findIndices,
        'prop_elemIndex, 'prop_elemIndices,

        -- Folding
        'prop_foldl, 'prop_foldl1, 'prop_foldl', 'prop_foldl1',
        'prop_foldr, 'prop_foldr1, 'prop_foldr', 'prop_foldr1',
        'prop_ifoldl, 'prop_ifoldl', 'prop_ifoldr, 'prop_ifoldr',
        'prop_ifoldM, 'prop_ifoldM', 'prop_ifoldM_, 'prop_ifoldM'_,

        -- Specialised folds
        'prop_all, 'prop_any,
        {- 'prop_maximumBy, 'prop_minimumBy,
        'prop_maxIndexBy, 'prop_minIndexBy, -}

        -- Monadic folds
        {- ... -}

        -- Monadic sequencing
        {- ... -}

        -- Scans
        'prop_prescanl, 'prop_prescanl',
        'prop_postscanl, 'prop_postscanl',
        'prop_scanl, 'prop_scanl', 'prop_scanl1, 'prop_scanl1',

        'prop_prescanr, 'prop_prescanr',
        'prop_postscanr, 'prop_postscanr',
        'prop_scanr, 'prop_scanr', 'prop_scanr1, 'prop_scanr1'
    ])
  where
    -- Prelude
    prop_eq :: P (v a -> v a -> Bool) = (==) `eq` (==)

    prop_length :: P (v a -> Int)     = V.length `eq` length
    prop_null   :: P (v a -> Bool)    = V.null `eq` null

    prop_empty  :: P (v a)            = V.empty `eq` []
    prop_singleton :: P (a -> v a)    = V.singleton `eq` singleton
    prop_replicate :: P (Int -> a -> v a)
              = (\n _ -> n < 1000) ===> V.replicate `eq` replicate
    prop_cons      :: P (a -> v a -> v a) = V.cons `eq` (:)
    prop_snoc      :: P (v a -> a -> v a) = V.snoc `eq` snoc
    prop_append    :: P (v a -> v a -> v a) = (V.++) `eq` (++)
    prop_concat    :: P ([v a] -> v a) = V.concat `eq` concat
    prop_force     :: P (v a -> v a)        = V.force `eq` id
    prop_generate  :: P (Int -> (Int -> a) -> v a)
              = (\n _ -> n < 1000) ===> V.generate `eq` Util.generate
    prop_iterateN  :: P (Int -> (a -> a) -> a -> v a)
              = (\n _ _ -> n < 1000) ===> V.iterateN `eq` (\n f -> take n . iterate f)

    prop_head      :: P (v a -> a) = not . V.null ===> V.head `eq` head
    prop_last      :: P (v a -> a) = not . V.null ===> V.last `eq` last
    prop_index        = \xs ->
                        not (V.null xs) ==>
                        forAll (choose (0, V.length xs-1)) $ \i ->
                        unP prop xs i
      where
        prop :: P (v a -> Int -> a) = (V.!) `eq` (!!)
    prop_safeIndex :: P (v a -> Int -> Maybe a) = (V.!?) `eq` fn
      where
        fn xs i = case drop i xs of
                    x:_ | i >= 0 -> Just x
                    _            -> Nothing
    prop_unsafeHead  :: P (v a -> a) = not . V.null ===> V.unsafeHead `eq` head
    prop_unsafeLast  :: P (v a -> a) = not . V.null ===> V.unsafeLast `eq` last
    prop_unsafeIndex  = \xs ->
                        not (V.null xs) ==>
                        forAll (choose (0, V.length xs-1)) $ \i ->
                        unP prop xs i
      where
        prop :: P (v a -> Int -> a) = V.unsafeIndex `eq` (!!)

    prop_slice        = \xs ->
                        forAll (choose (0, V.length xs))     $ \i ->
                        forAll (choose (0, V.length xs - i)) $ \n ->
                        unP prop i n xs
      where
        prop :: P (Int -> Int -> v a -> v a) = V.slice `eq` slice

    prop_tail :: P (v a -> v a) = not . V.null ===> V.tail `eq` tail
    prop_init :: P (v a -> v a) = not . V.null ===> V.init `eq` init
    prop_take :: P (Int -> v a -> v a) = V.take `eq` take
    prop_drop :: P (Int -> v a -> v a) = V.drop `eq` drop
    prop_splitAt :: P (Int -> v a -> (v a, v a)) = V.splitAt `eq` splitAt

    prop_accum = \f xs ->
                 forAll (index_value_pairs (V.length xs)) $ \ps ->
                 unP prop f xs ps
      where
        prop :: P ((a -> a -> a) -> v a -> [(Int,a)] -> v a)
          = V.accum `eq` accum

    prop_upd        = \xs ->
                        forAll (index_value_pairs (V.length xs)) $ \ps ->
                        unP prop xs ps
      where
        prop :: P (v a -> [(Int,a)] -> v a) = (V.//) `eq` (//)

    prop_backpermute  = \xs ->
                        forAll (indices (V.length xs)) $ \is ->
                        unP prop xs (V.fromList is)
      where
        prop :: P (v a -> v Int -> v a) = V.backpermute `eq` backpermute

    prop_reverse :: P (v a -> v a) = V.reverse `eq` reverse

    prop_map :: P ((a -> a) -> v a -> v a) = V.map `eq` map
    prop_zipWith :: P ((a -> a -> a) -> v a -> v a -> v a) = V.zipWith `eq` zipWith
    prop_zipWith3 :: P ((a -> a -> a -> a) -> v a -> v a -> v a -> v a)
             = V.zipWith3 `eq` zipWith3
    prop_imap :: P ((Int -> a -> a) -> v a -> v a) = V.imap `eq` imap
    prop_imapM :: P ((Int -> a -> Identity a) -> v a -> Identity (v a))
            = V.imapM `eq` imapM
    prop_imapM_ :: P ((Int -> a -> Writer [a] ()) -> v a -> Writer [a] ())
            = V.imapM_ `eq` imapM_
    prop_izipWith :: P ((Int -> a -> a -> a) -> v a -> v a -> v a) = V.izipWith `eq` izipWith
    prop_izipWithM :: P ((Int -> a -> a -> Identity a) -> v a -> v a -> Identity (v a))
            = V.izipWithM `eq` izipWithM
    prop_izipWithM_ :: P ((Int -> a -> a -> Writer [a] ()) -> v a -> v a -> Writer [a] ())
            = V.izipWithM_ `eq` izipWithM_
    prop_izipWith3 :: P ((Int -> a -> a -> a -> a) -> v a -> v a -> v a -> v a)
             = V.izipWith3 `eq` izipWith3

    prop_filter :: P ((a -> Bool) -> v a -> v a) = V.filter `eq` filter
    prop_ifilter :: P ((Int -> a -> Bool) -> v a -> v a) = V.ifilter `eq` ifilter
    prop_takeWhile :: P ((a -> Bool) -> v a -> v a) = V.takeWhile `eq` takeWhile
    prop_dropWhile :: P ((a -> Bool) -> v a -> v a) = V.dropWhile `eq` dropWhile
    prop_partition :: P ((a -> Bool) -> v a -> (v a, v a))
      = V.partition `eq` partition
    prop_span :: P ((a -> Bool) -> v a -> (v a, v a)) = V.span `eq` span
    prop_break :: P ((a -> Bool) -> v a -> (v a, v a)) = V.break `eq` break

    prop_elem    :: P (a -> v a -> Bool) = V.elem `eq` elem
    prop_notElem :: P (a -> v a -> Bool) = V.notElem `eq` notElem
    prop_find    :: P ((a -> Bool) -> v a -> Maybe a) = V.find `eq` find
    prop_findIndex :: P ((a -> Bool) -> v a -> Maybe Int)
      = V.findIndex `eq` findIndex
    prop_findIndices :: P ((a -> Bool) -> v a -> v Int)
        = V.findIndices `eq` findIndices
    prop_elemIndex :: P (a -> v a -> Maybe Int) = V.elemIndex `eq` elemIndex
    prop_elemIndices :: P (a -> v a -> v Int) = V.elemIndices `eq` elemIndices

    prop_foldl :: P ((a -> a -> a) -> a -> v a -> a) = V.foldl `eq` foldl
    prop_foldl1 :: P ((a -> a -> a) -> v a -> a)     = notNull2 ===>
                        V.foldl1 `eq` foldl1
    prop_foldl' :: P ((a -> a -> a) -> a -> v a -> a) = V.foldl' `eq` foldl'
    prop_foldl1' :: P ((a -> a -> a) -> v a -> a)     = notNull2 ===>
                        V.foldl1' `eq` foldl1'
    prop_foldr :: P ((a -> a -> a) -> a -> v a -> a) = V.foldr `eq` foldr
    prop_foldr1 :: P ((a -> a -> a) -> v a -> a)     = notNull2 ===>
                        V.foldr1 `eq` foldr1
    prop_foldr' :: P ((a -> a -> a) -> a -> v a -> a) = V.foldr' `eq` foldr
    prop_foldr1' :: P ((a -> a -> a) -> v a -> a)     = notNull2 ===>
                        V.foldr1' `eq` foldr1
    prop_ifoldl :: P ((a -> Int -> a -> a) -> a -> v a -> a)
        = V.ifoldl `eq` ifoldl
    prop_ifoldl' :: P ((a -> Int -> a -> a) -> a -> v a -> a)
        = V.ifoldl' `eq` ifoldl
    prop_ifoldr :: P ((Int -> a -> a -> a) -> a -> v a -> a)
        = V.ifoldr `eq` ifoldr
    prop_ifoldr' :: P ((Int -> a -> a -> a) -> a -> v a -> a)
        = V.ifoldr' `eq` ifoldr
    prop_ifoldM :: P ((a -> Int -> a -> Identity a) -> a -> v a -> Identity a)
        = V.ifoldM `eq` ifoldM
    prop_ifoldM' :: P ((a -> Int -> a -> Identity a) -> a -> v a -> Identity a)
        = V.ifoldM' `eq` ifoldM
    prop_ifoldM_ :: P ((() -> Int -> a -> Writer [a] ()) -> () -> v a -> Writer [a] ())
        = V.ifoldM_ `eq` ifoldM_
    prop_ifoldM'_ :: P ((() -> Int -> a -> Writer [a] ()) -> () -> v a -> Writer [a] ())
        = V.ifoldM'_ `eq` ifoldM_

    prop_all :: P ((a -> Bool) -> v a -> Bool) = V.all `eq` all
    prop_any :: P ((a -> Bool) -> v a -> Bool) = V.any `eq` any

    prop_prescanl :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.prescanl `eq` prescanl
    prop_prescanl' :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.prescanl' `eq` prescanl
    prop_postscanl :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.postscanl `eq` postscanl
    prop_postscanl' :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.postscanl' `eq` postscanl
    prop_scanl :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.scanl `eq` scanl
    prop_scanl' :: P ((a -> a -> a) -> a -> v a -> v a)
               = V.scanl' `eq` scanl
    prop_scanl1 :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanl1 `eq` scanl1
    prop_scanl1' :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanl1' `eq` scanl1

    prop_prescanr :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.prescanr `eq` prescanr
    prop_prescanr' :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.prescanr' `eq` prescanr
    prop_postscanr :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.postscanr `eq` postscanr
    prop_postscanr' :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.postscanr' `eq` postscanr
    prop_scanr :: P ((a -> a -> a) -> a -> v a -> v a)
                = V.scanr `eq` scanr
    prop_scanr' :: P ((a -> a -> a) -> a -> v a -> v a)
               = V.scanr' `eq` scanr
    prop_scanr1 :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanr1 `eq` scanr1
    prop_scanr1' :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanr1' `eq` scanr1

    prop_concatMap    = forAll arbitrary $ \xs ->
                        forAll (sized (\n -> resize (n `div` V.length xs) arbitrary)) $ \f -> unP prop f xs
      where
        prop :: P ((a -> v a) -> v a -> v a) = V.concatMap `eq` concatMap

    --prop_span         = (V.span :: (a -> Bool) -> v a -> (v a, v a))  `eq2` span
    --prop_break        = (V.break :: (a -> Bool) -> v a -> (v a, v a)) `eq2` break
    --prop_splitAt      = (V.splitAt :: Int -> v a -> (v a, v a))       `eq2` splitAt
    --prop_all          = (V.all :: (a -> Bool) -> v a -> Bool)         `eq2` all
    --prop_any          = (V.any :: (a -> Bool) -> v a -> Bool)         `eq2` any

    -- Data.List
    --prop_findIndices  = V.findIndices `eq2` (findIndices :: (a -> Bool) -> v a -> v Int)
    --prop_isPrefixOf   = V.isPrefixOf  `eq2` (isPrefixOf  :: v a -> v a -> Bool)
    --prop_elemIndex    = V.elemIndex   `eq2` (elemIndex   :: a -> v a -> Maybe Int)
    --prop_elemIndices  = V.elemIndices `eq2` (elemIndices :: a -> v a -> v Int)
    --
    --prop_mapAccumL  = eq3
    --    (V.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
    --
    --prop_mapAccumR  = eq3
    --    (V.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

    -- Because the vectors are strict, we need to be totally sure that the unfold eventually terminates. This
    -- is achieved by injecting our own bit of state into the unfold - the maximum number of unfolds allowed.
    limitUnfolds f (theirs, ours) | ours >= 0
                                  , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
                                  | otherwise                       = Nothing
    prop_unfoldr :: P (Int -> (Int -> Maybe (a,Int)) -> Int -> v a)
         = (\n f a -> V.unfoldr (limitUnfolds f) (a, n))
           `eq` (\n f a -> unfoldr (limitUnfolds f) (a, n))

    prop_constructN  = \f -> forAll (choose (0,20)) $ \n -> unP prop n f
      where
        prop :: P (Int -> (v a -> a) -> v a) = V.constructN `eq` constructN []

        constructN xs 0 _ = xs
        constructN xs n f = constructN (xs ++ [f xs]) (n-1) f

    prop_constructrN  = \f -> forAll (choose (0,20)) $ \n -> unP prop n f
      where
        prop :: P (Int -> (v a -> a) -> v a) = V.constructrN `eq` constructrN []

        constructrN xs 0 _ = xs
        constructrN xs n f = constructrN (f xs : xs) (n-1) f

testTuplyFunctions:: forall a v. (COMMON_CONTEXT(a, v), VECTOR_CONTEXT((a, a), v), VECTOR_CONTEXT((a, a, a), v)) => v a -> [Test]
testTuplyFunctions _ = $(testProperties ['prop_zip, 'prop_zip3, 'prop_unzip, 'prop_unzip3])
  where
    prop_zip    :: P (v a -> v a -> v (a, a))           = V.zip `eq` zip
    prop_zip3   :: P (v a -> v a -> v a -> v (a, a, a)) = V.zip3 `eq` zip3
    prop_unzip  :: P (v (a, a) -> (v a, v a))           = V.unzip `eq` unzip
    prop_unzip3 :: P (v (a, a, a) -> (v a, v a, v a))   = V.unzip3 `eq` unzip3

testOrdFunctions :: forall a v. (COMMON_CONTEXT(a, v), Ord a, Ord (v a)) => v a -> [Test]
testOrdFunctions _ = $(testProperties
  ['prop_compare,
   'prop_maximum, 'prop_minimum,
   'prop_minIndex, 'prop_maxIndex ])
  where
    prop_compare :: P (v a -> v a -> Ordering) = compare `eq` compare
    prop_maximum :: P (v a -> a) = not . V.null ===> V.maximum `eq` maximum
    prop_minimum :: P (v a -> a) = not . V.null ===> V.minimum `eq` minimum
    prop_minIndex :: P (v a -> Int) = not . V.null ===> V.minIndex `eq` minIndex
    prop_maxIndex :: P (v a -> Int) = not . V.null ===> V.maxIndex `eq` maxIndex

testEnumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Enum a, Ord a, Num a, Random a) => v a -> [Test]
testEnumFunctions _ = $(testProperties
  [ 'prop_enumFromN, 'prop_enumFromThenN,
    'prop_enumFromTo, 'prop_enumFromThenTo])
  where
    prop_enumFromN :: P (a -> Int -> v a)
      = (\_ n -> n < 1000)
        ===> V.enumFromN `eq` (\x n -> take n $ scanl (+) x $ repeat 1)

    prop_enumFromThenN :: P (a -> a -> Int -> v a)
      = (\_ _ n -> n < 1000)
        ===> V.enumFromStepN `eq` (\x y n -> take n $ scanl (+) x $ repeat y)

    prop_enumFromTo = \m ->
                      forAll (choose (-2,100)) $ \n ->
                      unP prop m (m+n)
      where
        prop  :: P (a -> a -> v a) = V.enumFromTo `eq` enumFromTo

    prop_enumFromThenTo = \i j ->
                          j /= i ==>
                          forAll (choose (ks i j)) $ \k ->
                          unP prop i j k
      where
        prop :: P (a -> a -> a -> v a) = V.enumFromThenTo `eq` enumFromThenTo

        ks i j | j < i     = (i-d*100, i+d*2)
               | otherwise = (i-d*2, i+d*100)
          where
            d = abs (j-i)

testMonoidFunctions :: forall a v. (COMMON_CONTEXT(a, v), Monoid (v a)) => v a -> [Test]
testMonoidFunctions _ = $(testProperties
  [ 'prop_mempty, 'prop_mappend, 'prop_mconcat ])
  where
    prop_mempty  :: P (v a)               = mempty `eq` mempty
    prop_mappend :: P (v a -> v a -> v a) = mappend `eq` mappend
    prop_mconcat :: P ([v a] -> v a)      = mconcat `eq` mconcat

testFunctorFunctions :: forall a v. (COMMON_CONTEXT(a, v), Functor v) => v a -> [Test]
testFunctorFunctions _ = $(testProperties
  [ 'prop_fmap ])
  where
    prop_fmap :: P ((a -> a) -> v a -> v a) = fmap `eq` fmap

testMonadFunctions :: forall a v. (COMMON_CONTEXT(a, v), Monad v) => v a -> [Test]
testMonadFunctions _ = $(testProperties
  [ 'prop_return, 'prop_bind ])
  where
    prop_return :: P (a -> v a) = return `eq` return
    prop_bind   :: P (v a -> (a -> v a) -> v a) = (>>=) `eq` (>>=)

testApplicativeFunctions :: forall a v. (COMMON_CONTEXT(a, v), V.Vector v (a -> a), Applicative.Applicative v) => v a -> [Test]
testApplicativeFunctions _ = $(testProperties
  [ 'prop_applicative_pure, 'prop_applicative_appl ])
  where
    prop_applicative_pure :: P (a -> v a)
      = Applicative.pure `eq` Applicative.pure
    prop_applicative_appl :: [a -> a] -> P (v a -> v a)
      = \fs -> (Applicative.<*>) (V.fromList fs) `eq` (Applicative.<*>) fs

testAlternativeFunctions :: forall a v. (COMMON_CONTEXT(a, v), Applicative.Alternative v) => v a -> [Test]
testAlternativeFunctions _ = $(testProperties
  [ 'prop_alternative_empty, 'prop_alternative_or ])
  where
    prop_alternative_empty :: P (v a) = Applicative.empty `eq` Applicative.empty
    prop_alternative_or :: P (v a -> v a -> v a)
      = (Applicative.<|>) `eq` (Applicative.<|>)

testBoolFunctions :: forall v. (COMMON_CONTEXT(Bool, v)) => v Bool -> [Test]
testBoolFunctions _ = $(testProperties ['prop_and, 'prop_or])
  where
    prop_and :: P (v Bool -> Bool) = V.and `eq` and
    prop_or  :: P (v Bool -> Bool) = V.or `eq` or

testNumFunctions :: forall a v. (COMMON_CONTEXT(a, v), Num a) => v a -> [Test]
testNumFunctions _ = $(testProperties ['prop_sum, 'prop_product])
  where
    prop_sum     :: P (v a -> a) = V.sum `eq` sum
    prop_product :: P (v a -> a) = V.product `eq` product

testNestedVectorFunctions :: forall a v. (COMMON_CONTEXT(a, v)) => v a -> [Test]
testNestedVectorFunctions _ = $(testProperties [])
  where
    -- Prelude
    --prop_concat       = (V.concat :: [v a] -> v a)                    `eq1` concat

    -- Data.List
    --prop_transpose    = V.transpose   `eq1` (transpose   :: [v a] -> [v a])
    --prop_group        = V.group       `eq1` (group       :: v a -> [v a])
    --prop_inits        = V.inits       `eq1` (inits       :: v a -> [v a])
    --prop_tails        = V.tails       `eq1` (tails       :: v a -> [v a])

testGeneralBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a) => Data.Vector.Vector a -> [Test]
testGeneralBoxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testTuplyFunctions,
        testNestedVectorFunctions,
        testMonoidFunctions,
        testFunctorFunctions,
        testMonadFunctions,
        testApplicativeFunctions,
        testAlternativeFunctions
    ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Vector), Ord a, Num a, Enum a, Random a) => Data.Vector.Vector a -> [Test]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]


testGeneralPrimitiveVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Primitive.Vector), Data.Vector.Primitive.Prim a, Ord a) => Data.Vector.Primitive.Vector a -> [Test]
testGeneralPrimitiveVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testNumericPrimitiveVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Primitive.Vector), Data.Vector.Primitive.Prim a, Ord a, Num a, Enum a, Random a) => Data.Vector.Primitive.Vector a -> [Test]
testNumericPrimitiveVector dummy = concatMap ($ dummy)
 [
   testGeneralPrimitiveVector
 , testNumFunctions
 , testEnumFunctions
 ]


testGeneralStorableVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Storable.Vector), Data.Vector.Storable.Storable a, Ord a) => Data.Vector.Storable.Vector a -> [Test]
testGeneralStorableVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testNumericStorableVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Storable.Vector), Data.Vector.Storable.Storable a, Ord a, Num a, Enum a, Random a) => Data.Vector.Storable.Vector a -> [Test]
testNumericStorableVector dummy = concatMap ($ dummy)
  [
    testGeneralStorableVector
  , testNumFunctions
  , testEnumFunctions
  ]


testGeneralUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a) => Data.Vector.Unboxed.Vector a -> [Test]
testGeneralUnboxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions
    ]

testUnitUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testBoolFunctions
  ]

testNumericUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a, Num a, Enum a, Random a) => Data.Vector.Unboxed.Vector a -> [Test]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

testTupleUnboxedVector :: forall a. (COMMON_CONTEXT(a, Data.Vector.Unboxed.Vector), Data.Vector.Unboxed.Unbox a, Ord a) => Data.Vector.Unboxed.Vector a -> [Test]
testTupleUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

tests = [
        testGroup "Data.Vector.Vector (Bool)"           (testBoolBoxedVector      (undefined :: Data.Vector.Vector Bool)),
        testGroup "Data.Vector.Vector (Int)"            (testNumericBoxedVector   (undefined :: Data.Vector.Vector Int)),

        testGroup "Data.Vector.Primitive.Vector (Int)"    (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)),
        testGroup "Data.Vector.Primitive.Vector (Double)" (testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Double)),

        testGroup "Data.Vector.Storable.Vector (Int)"    (testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Int)),
        testGroup "Data.Vector.Storable.Vector (Double)" (testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Double)),

        testGroup "Data.Vector.Unboxed.Vector ()"       (testUnitUnboxedVector (undefined :: Data.Vector.Unboxed.Vector ())),
        testGroup "Data.Vector.Unboxed.Vector (Bool)"       (testBoolUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Bool)),
        testGroup "Data.Vector.Unboxed.Vector (Int)"    (testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Int)),
        testGroup "Data.Vector.Unboxed.Vector (Double)" (testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Double)),
       testGroup "Data.Vector.Unboxed.Vector (Int,Bool)" (testTupleUnboxedVector (undefined :: Data.Vector.Unboxed.Vector (Int,Bool))),
         testGroup "Data.Vector.Unboxed.Vector (Int,Bool,Int)" (testTupleUnboxedVector (undefined :: Data.Vector.Unboxed.Vector (Int,Bool,Int)))

    ]

