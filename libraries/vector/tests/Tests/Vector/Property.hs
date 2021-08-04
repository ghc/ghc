{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Property
  ( CommonContext
  , VanillaContext
  , VectorContext
  , testSanity
  , testPolymorphicFunctions
  , testTuplyFunctions
  , testOrdFunctions
  , testEnumFunctions
  , testMonoidFunctions
  , testFunctorFunctions
  , testMonadFunctions
  , testApplicativeFunctions
  , testAlternativeFunctions
  , testSequenceFunctions
  , testBoolFunctions
  , testNumFunctions
  , testNestedVectorFunctions
  , testDataFunctions
  -- re-exports
  , Data
  , Random
  , Test
  ) where

import Boilerplater
import Utilities as Util hiding (limitUnfolds)

import Control.Monad
import Control.Monad.ST
import qualified Data.Traversable as T (Traversable(..))
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Identity
import Data.Orphans ()
import Data.Foldable (foldrM)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Fusion.Bundle as S

import Test.QuickCheck

import Test.Tasty
import Test.Tasty.QuickCheck hiding (testProperties)

import Text.Show.Functions ()
import Data.List

import Data.Monoid

import qualified Control.Applicative as Applicative
import System.Random       (Random)

import Data.Functor.Identity
import Control.Monad.Trans.Writer

import Control.Monad.Zip

import Data.Data

import qualified Data.List.NonEmpty as DLE
import Data.Semigroup (Semigroup(..))

type CommonContext  a v = (VanillaContext a, VectorContext a v)
type VanillaContext a   = ( Eq a , Show a, Arbitrary a, CoArbitrary a
                          , TestData a, Model a ~ a, EqTest a ~ Property)
type VectorContext  a v = ( Eq (v a), Show (v a), Arbitrary (v a), CoArbitrary (v a)
                          , TestData (v a), Model (v a) ~ [a],  EqTest (v a) ~ Property, V.Vector v a)

-- | migration hack for moving from TestFramework to Tasty
type Test = TestTree
-- TODO: implement Vector equivalents of list functions for some of the commented out properties

-- TODO: add tests for the other extra functions
-- IVector exports still needing tests:
--  copy,
--  new,
--  unsafeSlice, unsafeIndex,

testSanity :: forall a v. (CommonContext a v) => v a -> [Test]
{-# INLINE testSanity #-}
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

testPolymorphicFunctions :: forall a v. (CommonContext a v, VectorContext Int v) => v a -> [Test]
-- FIXME: inlining of unboxed properties blows up the memory during compilation. See #272
--{-# INLINE testPolymorphicFunctions #-}
testPolymorphicFunctions _ = $(testProperties [
        'prop_eq,

        -- Length information
        'prop_length, 'prop_null,

        -- Indexing
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
        'prop_generate, 'prop_iterateN, 'prop_iterateNM,
        'prop_generateM, 'prop_replicateM,

        -- Monadic initialisation (FIXME)
        'prop_create, 'prop_createT,

        -- Unfolding
        'prop_unfoldr, 'prop_unfoldrN, 'prop_unfoldrExactN,
        'prop_unfoldrM, 'prop_unfoldrNM, 'prop_unfoldrExactNM,
        'prop_constructN, 'prop_constructrN,

        -- Concatenation (FIXME)
        'prop_cons, 'prop_snoc, 'prop_append,
        'prop_concat,

        -- Restricting memory usage
        'prop_force,


        -- Bulk updates (FIXME)
        'prop_upd,
        {- 'prop_update_,
        'prop_unsafeUpd, 'prop_unsafeUpdate, 'prop_unsafeUpdate_, -}

        -- Accumulations (FIXME)
        'prop_accum,
        {- 'prop_accumulate, 'prop_accumulate_,
        'prop_unsafeAccum, 'prop_unsafeAccumulate, 'prop_unsafeAccumulate_, -}

        -- Permutations
        'prop_reverse, 'prop_backpermute,
        {- 'prop_unsafeBackpermute, -}

        -- Mapping
        'prop_map, 'prop_imap, 'prop_concatMap,

        -- Monadic mapping
        'prop_mapM, 'prop_mapM_, 'prop_forM, 'prop_forM_,
        'prop_imapM, 'prop_imapM_,

        -- Zipping
        'prop_zipWith, 'prop_zipWith3,
        'prop_izipWith, 'prop_izipWith3,
        'prop_izipWithM, 'prop_izipWithM_,

        -- Monadic zipping
        'prop_zipWithM, 'prop_zipWithM_,

        -- Filtering
        'prop_filter, 'prop_ifilter, 'prop_filterM,
        'prop_uniq,
        'prop_mapMaybe, 'prop_imapMaybe,
        'prop_takeWhile, 'prop_dropWhile,

        -- Paritioning
        'prop_partition, {- 'prop_unstablePartition, -}
        'prop_partitionWith,
        'prop_span, 'prop_break,

        -- Searching
        'prop_elem, 'prop_notElem,
        'prop_find, 'prop_findIndex, 'prop_findIndexR, 'prop_findIndices,
        'prop_elemIndex, 'prop_elemIndices,

        -- Folding
        'prop_foldl, 'prop_foldl1, 'prop_foldl', 'prop_foldl1',
        'prop_foldr, 'prop_foldr1, 'prop_foldr', 'prop_foldr1',
        'prop_ifoldl, 'prop_ifoldl', 'prop_ifoldr, 'prop_ifoldr',
        'prop_ifoldM, 'prop_ifoldM', 'prop_ifoldM_, 'prop_ifoldM'_,

        -- Specialised folds
        'prop_all, 'prop_any,

        -- Scans
        'prop_prescanl, 'prop_prescanl',
        'prop_postscanl, 'prop_postscanl',
        'prop_scanl, 'prop_scanl', 'prop_scanl1, 'prop_scanl1',
        'prop_iscanl, 'prop_iscanl',

        'prop_prescanr, 'prop_prescanr',
        'prop_postscanr, 'prop_postscanr',
        'prop_scanr, 'prop_scanr', 'prop_scanr1, 'prop_scanr1',
        'prop_iscanr, 'prop_iscanr',

        -- Mutable API
        'prop_mut_read, 'prop_mut_write, 'prop_mut_modify,

        'prop_mut_generate, 'prop_mut_generateM,
        'prop_mut_mapM_, 'prop_mut_imapM_, 'prop_mut_forM_, 'prop_mut_iforM_,
        'prop_mut_foldr, 'prop_mut_foldr', 'prop_mut_foldl, 'prop_mut_foldl',
        'prop_mut_ifoldr, 'prop_mut_ifoldr', 'prop_mut_ifoldl, 'prop_mut_ifoldl',
        'prop_mut_foldM, 'prop_mut_foldM', 'prop_mut_foldrM, 'prop_mut_foldrM',
        'prop_mut_ifoldM, 'prop_mut_ifoldM', 'prop_mut_ifoldrM, 'prop_mut_ifoldrM'
    ])
  where
    -- Prelude
    prop_eq :: P (v a -> v a -> Bool) = (==) `eq` (==)

    prop_length :: P (v a -> Int)     = V.length `eq` length
    prop_null   :: P (v a -> Bool)    = V.null `eq` null

    prop_empty  :: P (v a)            = V.empty `eq` []
    prop_singleton :: P (a -> v a)    = V.singleton `eq` Util.singleton
    prop_replicate :: P (Int -> a -> v a)
              = (\n _ -> n < 1000) ===> V.replicate `eq` replicate
    prop_replicateM :: P (Int -> Writer [a] a -> Writer [a] (v a))
              = (\n _ -> n < 1000) ===> V.replicateM `eq` replicateM
    prop_cons      :: P (a -> v a -> v a) = V.cons `eq` (:)
    prop_snoc      :: P (v a -> a -> v a) = V.snoc `eq` snoc
    prop_append    :: P (v a -> v a -> v a) = (V.++) `eq` (++)
    prop_concat    :: P ([v a] -> v a) = V.concat `eq` concat
    prop_force     :: P (v a -> v a)        = V.force `eq` id
    prop_generate  :: P (Int -> (Int -> a) -> v a)
              = (\n _ -> n < 1000) ===> V.generate `eq` Util.generate
    prop_generateM  :: P (Int -> (Int -> Writer [a] a) -> Writer [a] (v a))
              = (\n _ -> n < 1000) ===> V.generateM `eq` Util.generateM
    prop_iterateN  :: P (Int -> (a -> a) -> a -> v a)
              = (\n _ _ -> n < 1000) ===> V.iterateN `eq` (\n f -> take n . iterate f)
    prop_iterateNM :: P (Int -> (a -> Writer [Int] a) -> a -> Writer [Int] (v a))
              = (\n _ _ -> n < 1000) ===> V.iterateNM `eq` Util.iterateNM
    prop_create :: P (v a -> v a)
    prop_create = (\v -> V.create (V.thaw v)) `eq` id
    prop_createT :: P ((a, v a) -> (a, v a))
    prop_createT = (\v -> V.createT (T.mapM V.thaw v)) `eq` id

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
    prop_mapM :: P ((a -> Identity a) -> v a -> Identity (v a))
            = V.mapM `eq` mapM
    prop_mapM_ :: P ((a -> Writer [a] ()) -> v a -> Writer [a] ())
            = V.mapM_ `eq` mapM_
    prop_forM :: P (v a -> (a -> Identity a) -> Identity (v a))
            = V.forM `eq` forM
    prop_forM_ :: P (v a -> (a -> Writer [a] ()) -> Writer [a] ())
            = V.forM_ `eq` forM_
    prop_zipWith :: P ((a -> a -> a) -> v a -> v a -> v a) = V.zipWith `eq` zipWith
    prop_zipWith3 :: P ((a -> a -> a -> a) -> v a -> v a -> v a -> v a)
             = V.zipWith3 `eq` zipWith3
    prop_imap :: P ((Int -> a -> a) -> v a -> v a) = V.imap `eq` imap
    prop_imapM :: P ((Int -> a -> Identity a) -> v a -> Identity (v a))
            = V.imapM `eq` imapM
    prop_imapM_ :: P ((Int -> a -> Writer [a] ()) -> v a -> Writer [a] ())
            = V.imapM_ `eq` imapM_
    prop_izipWith :: P ((Int -> a -> a -> a) -> v a -> v a -> v a) = V.izipWith `eq` izipWith
    prop_zipWithM :: P ((a -> a -> Identity a) -> v a -> v a -> Identity (v a))
            = V.zipWithM `eq` zipWithM
    prop_zipWithM_ :: P ((a -> a -> Writer [a] ()) -> v a -> v a -> Writer [a] ())
            = V.zipWithM_ `eq` zipWithM_
    prop_izipWithM :: P ((Int -> a -> a -> Identity a) -> v a -> v a -> Identity (v a))
            = V.izipWithM `eq` izipWithM
    prop_izipWithM_ :: P ((Int -> a -> a -> Writer [a] ()) -> v a -> v a -> Writer [a] ())
            = V.izipWithM_ `eq` izipWithM_
    prop_izipWith3 :: P ((Int -> a -> a -> a -> a) -> v a -> v a -> v a -> v a)
             = V.izipWith3 `eq` izipWith3

    prop_filter :: P ((a -> Bool) -> v a -> v a) = V.filter `eq` filter
    prop_ifilter :: P ((Int -> a -> Bool) -> v a -> v a) = V.ifilter `eq` ifilter
    prop_filterM :: P ((a -> Writer [a] Bool) -> v a -> Writer [a] (v a)) = V.filterM `eq` filterM
    prop_mapMaybe :: P ((a -> Maybe a) -> v a -> v a) = V.mapMaybe `eq` mapMaybe
    prop_imapMaybe :: P ((Int -> a -> Maybe a) -> v a -> v a) = V.imapMaybe `eq` imapMaybe
    prop_takeWhile :: P ((a -> Bool) -> v a -> v a) = V.takeWhile `eq` takeWhile
    prop_dropWhile :: P ((a -> Bool) -> v a -> v a) = V.dropWhile `eq` dropWhile
    prop_partition :: P ((a -> Bool) -> v a -> (v a, v a))
      = V.partition `eq` partition
    prop_partitionWith :: P ((a -> Either a a) -> v a -> (v a, v a))
      = V.partitionWith `eq` partitionWith
    prop_span :: P ((a -> Bool) -> v a -> (v a, v a)) = V.span `eq` span
    prop_break :: P ((a -> Bool) -> v a -> (v a, v a)) = V.break `eq` break

    prop_elem    :: P (a -> v a -> Bool) = V.elem `eq` elem
    prop_notElem :: P (a -> v a -> Bool) = V.notElem `eq` notElem
    prop_find    :: P ((a -> Bool) -> v a -> Maybe a) = V.find `eq` find
    prop_findIndex :: P ((a -> Bool) -> v a -> Maybe Int)
      = V.findIndex `eq` findIndex
    prop_findIndexR :: P ((a -> Bool) -> v a -> Maybe Int)
      = V.findIndexR `eq` \p l -> case filter (p . snd) . reverse $ zip [0..] l of
                                     (i,_):_ -> Just i
                                     []      -> Nothing
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
    prop_iscanl :: P ((Int -> a -> a -> a) -> a -> v a -> v a)
                = V.iscanl `eq` iscanl
    prop_iscanl' :: P ((Int -> a -> a -> a) -> a -> v a -> v a)
               = V.iscanl' `eq` iscanl

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
    prop_iscanr :: P ((Int -> a -> a -> a) -> a -> v a -> v a)
                = V.iscanr `eq` iscanr
    prop_iscanr' :: P ((Int -> a -> a -> a) -> a -> v a -> v a)
               = V.iscanr' `eq` iscanr
    prop_scanr1 :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanr1 `eq` scanr1
    prop_scanr1' :: P ((a -> a -> a) -> v a -> v a) = notNull2 ===>
                 V.scanr1' `eq` scanr1

    prop_concatMap    = forAll arbitrary $ \xs ->
                        forAll (sized (\n -> resize (n `div` V.length xs) arbitrary)) $ \f -> unP prop f xs
      where
        prop :: P ((a -> v a) -> v a -> v a) = V.concatMap `eq` concatMap

    prop_uniq :: P (v a -> v a)
      = V.uniq `eq` (map head . group)

    -- Data.List
    --prop_mapAccumL  = eq3
    --    (V.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
    --
    --prop_mapAccumR  = eq3
    --    (V.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    --    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

    -- Because the vectors are strict, we need to be totally sure that the unfold eventually terminates. This
    -- is achieved by injecting our own bit of state into the unfold - the maximum number of unfolds allowed.
    limitUnfolds f (theirs, ours)
        | ours > 0
        , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
        | otherwise                       = Nothing
    limitUnfoldsM f (theirs, ours)
        | ours >  0 = do r <- f theirs
                         return $ (\(a,b) -> (a,(b,ours - 1))) `fmap` r
        | otherwise = return Nothing


    prop_unfoldr :: P (Int -> (Int -> Maybe (a,Int)) -> Int -> v a)
         = (\n f a -> V.unfoldr (limitUnfolds f) (a, n))
           `eq` (\n f a -> unfoldr (limitUnfolds f) (a, n))
    prop_unfoldrN :: P (Int -> (Int -> Maybe (a,Int)) -> Int -> v a)
         = V.unfoldrN `eq` (\n f a -> unfoldr (limitUnfolds f) (a, n))
    prop_unfoldrExactN :: P (Int -> (Int -> (a,Int)) -> Int -> v a)
         = V.unfoldrExactN `eq` (\n f a -> unfoldr (limitUnfolds (Just . f)) (a, n))
    prop_unfoldrM :: P (Int -> (Int -> Writer [Int] (Maybe (a,Int))) -> Int -> Writer [Int] (v a))
         = (\n f a -> V.unfoldrM (limitUnfoldsM f) (a,n))
           `eq` (\n f a -> Util.unfoldrM (limitUnfoldsM f) (a, n))
    prop_unfoldrNM :: P (Int -> (Int -> Writer [Int] (Maybe (a,Int))) -> Int -> Writer [Int] (v a))
         = V.unfoldrNM `eq` (\n f a -> Util.unfoldrM (limitUnfoldsM f) (a, n))
    prop_unfoldrExactNM :: P (Int -> (Int -> Writer [Int] (a,Int)) -> Int -> Writer [Int] (v a))
         = V.unfoldrExactNM `eq` (\n f a -> Util.unfoldrM (limitUnfoldsM (liftM Just . f)) (a, n))

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

    prop_mut_foldr :: P ((a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.foldr f z =<< V.thaw v) `eq` foldr
    prop_mut_foldr' :: P ((a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.foldr' f z =<< V.thaw v) `eq` foldr
    prop_mut_foldl :: P ((a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.foldl f z =<< V.thaw v) `eq` foldl
    prop_mut_foldl' :: P ((a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.foldl' f z =<< V.thaw v) `eq` foldl'
    prop_mut_ifoldr :: P ((Int -> a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.ifoldr f z =<< V.thaw v) `eq` ifoldr
    prop_mut_ifoldr' :: P ((Int -> a -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.ifoldr' f z =<< V.thaw v) `eq` ifoldr
    prop_mut_ifoldl :: P ((a -> Int -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.ifoldl f z =<< V.thaw v) `eq` ifoldl
    prop_mut_ifoldl' :: P ((a -> Int -> a -> a) -> a -> v a -> a) =
      (\f z v -> runST $ MV.ifoldl' f z =<< V.thaw v) `eq` ifoldl

    prop_mut_foldM :: P ((a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.foldM (\b -> pure . runIdentity . f b) z =<< V.thaw v)
      `eq` foldM
    prop_mut_foldM' :: P ((a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.foldM' (\b -> pure . runIdentity . f b) z =<< V.thaw v)
      `eq` foldM
    prop_mut_foldrM :: P ((a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.foldrM (\a -> pure . runIdentity . f a) z =<< V.thaw v)
      `eq`
      foldrM
    prop_mut_foldrM' :: P ((a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.foldrM' (\a b -> pure $ runIdentity $ f a b) z =<< V.thaw v)
      `eq`
      foldrM

    prop_mut_read = \xs ->
      not (V.null xs) ==>
      forAll (choose (0, V.length xs-1)) $ \i ->
      unP prop xs i
      where
        prop :: P (v a -> Int -> a) = (\v i -> runST $ do mv <- V.thaw v
                                                          MV.read mv i
                                      ) `eq` (!!)
    prop_mut_write = \xs ->
      not (V.null xs) ==>
      forAll (choose (0, V.length xs-1)) $ \i ->
      unP prop xs i
      where
        prop :: P (v a -> Int -> a -> v a) = (\v i a -> runST $ do mv <- V.thaw v
                                                                   MV.write mv i a
                                                                   V.freeze mv
                                             ) `eq` writeList
    prop_mut_modify = \xs f ->
      not (V.null xs) ==>
      forAll (choose (0, V.length xs-1)) $ \i ->
      unP prop xs f i
      where
        prop :: P (v a -> (a -> a) -> Int -> v a)
          = (\v f i -> runST $ do mv <- V.thaw v
                                  MV.modify mv f i
                                  V.freeze mv
            ) `eq` modifyList



    prop_mut_generate :: P (Int -> (Int -> a) -> v a)
      = (\n _ -> n < 1000) ===> (\n f -> runST $ V.freeze =<< MV.generate n f)
      `eq` Util.generate
    prop_mut_generateM :: P (Int -> (Int -> Writer [a] a) -> Writer [a] (v a))
      = (\n _ -> n < 1000) ===> (\n f -> liftRunST $ V.freeze =<< MV.generateM n (hoistST . f))
      `eq` Util.generateM

    prop_mut_ifoldM :: P ((a -> Int -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.ifoldM (\b i -> pure . runIdentity . f b i) z =<< V.thaw v)
      `eq` ifoldM
    prop_mut_ifoldM' :: P ((a -> Int -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.ifoldM' (\b i -> pure . runIdentity . f b i) z =<< V.thaw v)
      `eq` ifoldM
    prop_mut_ifoldrM :: P ((Int -> a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.ifoldrM (\i b -> pure . runIdentity . f i b) z =<< V.thaw v)
      `eq`
      ifoldrM
    prop_mut_ifoldrM' :: P ((Int -> a -> a -> Identity a) -> a -> v a -> Identity a)
      = (\f z v -> Identity $ runST $ MV.ifoldrM' (\i b -> pure . runIdentity . f i b) z =<< V.thaw v)
      `eq`
      ifoldrM

    prop_mut_forM_ :: P (v a -> (a -> Writer [a] ()) -> Writer [a] ())
      = (\v f -> liftRunST $ do mv <- V.thaw v
                                MV.forM_ mv (hoistST . f))
      `eq` flip mapM_
    prop_mut_iforM_ :: P (v a -> (Int -> a -> Writer [a] ()) -> Writer [a] ())
      = (\v f -> liftRunST $ do mv <- V.thaw v
                                MV.iforM_ mv (\i x -> hoistST $ f i x))
      `eq` flip imapM_
    prop_mut_mapM_ :: P ((a -> Writer [a] ()) -> v a -> Writer [a] ())
      = (\f v -> liftRunST $ MV.mapM_ (hoistST . f) =<< V.thaw v) `eq` mapM_
    prop_mut_imapM_ :: P ((Int -> a -> Writer [a] ()) -> v a -> Writer [a] ())
      = (\f v -> liftRunST $ MV.imapM_ (\i x -> hoistST $ f i x) =<< V.thaw v) `eq` imapM_


liftRunST :: (forall s. WriterT w (ST s) a) -> Writer w a
liftRunST m = WriterT $ Identity $ runST $ runWriterT m

hoistST :: Writer w a -> WriterT w (ST s) a
hoistST = WriterT . pure . runWriter

-- copied from GHC source code
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

testTuplyFunctions
  :: forall a v. ( CommonContext a v
                 , VectorContext (a, a)    v
                 , VectorContext (a, a, a) v
                 , VectorContext (Int, a)  v
                 )
  => v a -> [Test]
{-# INLINE testTuplyFunctions #-}
testTuplyFunctions _ = $(testProperties [ 'prop_zip, 'prop_zip3
                                        , 'prop_unzip, 'prop_unzip3
                                        , 'prop_indexed
                                        , 'prop_update
                                        ])
  where
    prop_zip     :: P (v a -> v a -> v (a, a))           = V.zip `eq` zip
    prop_zip3    :: P (v a -> v a -> v a -> v (a, a, a)) = V.zip3 `eq` zip3
    prop_unzip   :: P (v (a, a) -> (v a, v a))           = V.unzip `eq` unzip
    prop_unzip3  :: P (v (a, a, a) -> (v a, v a, v a))   = V.unzip3 `eq` unzip3
    prop_indexed :: P (v a -> v (Int, a))                = V.indexed `eq` (\xs -> [0..] `zip` xs)
    prop_update = \xs ->
      forAll (index_value_pairs (V.length xs)) $ \ps ->
      unP prop xs ps
      where
        prop :: P (v a -> [(Int,a)] -> v a) = (V.//) `eq` (//)

testOrdFunctions :: forall a v. (CommonContext a v, Ord a, Ord (v a)) => v a -> [Test]
{-# INLINE testOrdFunctions #-}
testOrdFunctions _ = $(testProperties
  ['prop_compare,
   'prop_maximum, 'prop_minimum,
   'prop_minIndex, 'prop_maxIndex,
   'prop_maximumBy, 'prop_minimumBy,
   'prop_maxIndexBy, 'prop_minIndexBy,
   'prop_ListLastMaxIndexWins, 'prop_FalseListFirstMaxIndexWins ])
  where
    prop_compare :: P (v a -> v a -> Ordering) = compare `eq` compare
    prop_maximum :: P (v a -> a) = not . V.null ===> V.maximum `eq` maximum
    prop_minimum :: P (v a -> a) = not . V.null ===> V.minimum `eq` minimum
    prop_minIndex :: P (v a -> Int) = not . V.null ===> V.minIndex `eq` minIndex
    prop_maxIndex :: P (v a -> Int) = not . V.null ===> V.maxIndex `eq` listMaxIndexFMW
    prop_maximumBy :: P (v a -> a) =
      not . V.null ===> V.maximumBy compare `eq` maximum
    prop_minimumBy :: P (v a -> a) =
      not . V.null ===> V.minimumBy compare `eq` minimum
    prop_maxIndexBy :: P (v a -> Int) =
      not . V.null ===> V.maxIndexBy compare `eq`  listMaxIndexFMW
                                          ---   (maxIndex)
    prop_ListLastMaxIndexWins ::  P (v a -> Int) =
        not . V.null ===> ( maxIndex . V.toList) `eq` listMaxIndexLMW
    prop_FalseListFirstMaxIndexWinsDesc ::  P (v a -> Int) =
        (\x -> not $ V.null x && (V.uniq x /= x ) )===> ( maxIndex . V.toList) `eq` listMaxIndexFMW
    prop_FalseListFirstMaxIndexWins :: Property
    prop_FalseListFirstMaxIndexWins = expectFailure prop_FalseListFirstMaxIndexWinsDesc
    prop_minIndexBy :: P (v a -> Int) =
      not . V.null ===> V.minIndexBy compare `eq` minIndex

listMaxIndexFMW :: Ord a => [a] -> Int
listMaxIndexFMW  = ( fst  . extractFMW .  sconcat . DLE.fromList . fmap FMW . zip [0 :: Int ..])

listMaxIndexLMW :: Ord a => [a] -> Int
listMaxIndexLMW = ( fst  . extractLMW .  sconcat . DLE.fromList . fmap LMW . zip [0 :: Int ..])

newtype LastMaxWith a i = LMW {extractLMW:: (i,a)}
    deriving(Eq,Show,Read)
instance (Ord a) => Semigroup  (LastMaxWith a i)   where
    (<>) x y | snd (extractLMW x) > snd (extractLMW y) = x
             | snd (extractLMW x) < snd (extractLMW y) = y
             | otherwise = y
newtype FirstMaxWith a i = FMW {extractFMW:: (i,a)}
    deriving(Eq,Show,Read)
instance (Ord a) => Semigroup  (FirstMaxWith a i)   where
    (<>) x y | snd (extractFMW x) > snd (extractFMW y) = x
             | snd (extractFMW x) < snd (extractFMW y) = y
             | otherwise = x


testEnumFunctions :: forall a v. (CommonContext a v, Enum a, Ord a, Num a, Random a) => v a -> [Test]
{-# INLINE testEnumFunctions #-}
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

testMonoidFunctions :: forall a v. (CommonContext a v, Monoid (v a)) => v a -> [Test]
{-# INLINE testMonoidFunctions #-}
testMonoidFunctions _ = $(testProperties
  [ 'prop_mempty, 'prop_mappend, 'prop_mconcat ])
  where
    prop_mempty  :: P (v a)               = mempty `eq` mempty
    prop_mappend :: P (v a -> v a -> v a) = mappend `eq` mappend
    prop_mconcat :: P ([v a] -> v a)      = mconcat `eq` mconcat

testFunctorFunctions :: forall a v. (CommonContext a v, Functor v) => v a -> [Test]
{-# INLINE testFunctorFunctions #-}
testFunctorFunctions _ = $(testProperties
  [ 'prop_fmap ])
  where
    prop_fmap :: P ((a -> a) -> v a -> v a) = fmap `eq` fmap

testMonadFunctions :: forall a v. (CommonContext a v, VectorContext (a, a) v, MonadZip v) => v a -> [Test]
{-# INLINE testMonadFunctions #-}
testMonadFunctions _ = $(testProperties [ 'prop_return, 'prop_bind
                                        , 'prop_mzip, 'prop_munzip
                                        ])
  where
    prop_return :: P (a -> v a) = return `eq` return
    prop_bind   :: P (v a -> (a -> v a) -> v a) = (>>=) `eq` (>>=)
    prop_mzip   :: P (v a -> v a -> v (a, a)) = mzip `eq` zip
    prop_munzip :: P (v (a, a) -> (v a, v a)) = munzip `eq` unzip

testSequenceFunctions
  :: forall a v. ( CommonContext a v
                 , Model (v (Writer [a] a)) ~ [Writer [a] a]
                 , V.Vector v (Writer [a] a)
                 , Arbitrary (v (Writer [a] a))
                 , Show      (v (Writer [a] a))
                 , TestData  (v (Writer [a] a))
                 )
  => v a -> [Test]
testSequenceFunctions _ = $(testProperties [ 'prop_sequence, 'prop_sequence_
                                           ])
  where
    prop_sequence :: P (v (Writer [a] a) -> Writer [a] (v a))
      = V.sequence `eq` sequence
    prop_sequence_ :: P (v (Writer [a] a) -> Writer [a] ())
      = V.sequence_ `eq` sequence_

testApplicativeFunctions :: forall a v. (CommonContext a v, V.Vector v (a -> a), Applicative.Applicative v) => v a -> [Test]
{-# INLINE testApplicativeFunctions #-}
testApplicativeFunctions _ = $(testProperties
  [ 'prop_applicative_pure, 'prop_applicative_appl ])
  where
    prop_applicative_pure :: P (a -> v a)
      = Applicative.pure `eq` Applicative.pure
    prop_applicative_appl :: [a -> a] -> P (v a -> v a)
      = \fs -> (Applicative.<*>) (V.fromList fs) `eq` (Applicative.<*>) fs

testAlternativeFunctions :: forall a v. (CommonContext a v, Applicative.Alternative v) => v a -> [Test]
{-# INLINE testAlternativeFunctions #-}
testAlternativeFunctions _ = $(testProperties
  [ 'prop_alternative_empty, 'prop_alternative_or ])
  where
    prop_alternative_empty :: P (v a) = Applicative.empty `eq` Applicative.empty
    prop_alternative_or :: P (v a -> v a -> v a)
      = (Applicative.<|>) `eq` (Applicative.<|>)

testBoolFunctions :: forall v. (CommonContext Bool v) => v Bool -> [Test]
{-# INLINE testBoolFunctions #-}
testBoolFunctions _ = $(testProperties ['prop_and, 'prop_or])
  where
    prop_and :: P (v Bool -> Bool) = V.and `eq` and
    prop_or  :: P (v Bool -> Bool) = V.or `eq` or

testNumFunctions :: forall a v. (CommonContext a v, Num a) => v a -> [Test]
{-# INLINE testNumFunctions #-}
testNumFunctions _ = $(testProperties ['prop_sum, 'prop_product])
  where
    prop_sum     :: P (v a -> a) = V.sum `eq` sum
    prop_product :: P (v a -> a) = V.product `eq` product

testNestedVectorFunctions :: forall a v. (CommonContext a v) => v a -> [Test]
{-# INLINE testNestedVectorFunctions #-}
testNestedVectorFunctions _ = $(testProperties
  [ 'prop_concat
  ])
  where
    prop_concat :: P ([v a] -> v a) = V.concat `eq` concat

testDataFunctions :: forall a v. (CommonContext a v, Data a, Data (v a)) => v a -> [Test]
{-# INLINE testDataFunctions #-}
testDataFunctions _ = $(testProperties ['prop_glength])
  where
    prop_glength :: P (v a -> Int) = glength `eq` glength
      where
        glength :: Data b => b -> Int
        glength xs = gmapQl (+) 0 toA xs

        toA :: Data b => b -> Int
        toA x = maybe (glength x) (const 1) (cast x :: Maybe a)
