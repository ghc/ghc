module Tests.Bundle ( tests ) where

import Boilerplater
import Utilities

import qualified Data.Vector.Fusion.Bundle as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Show.Functions ()
import Data.List           (foldl', foldl1', unfoldr, find, findIndex)
import System.Random       (Random)

#define COMMON_CONTEXT(a) \
 VANILLA_CONTEXT(a)

#define VANILLA_CONTEXT(a) \
  Eq a,     Show a,     Arbitrary a,     CoArbitrary a,     TestData a,     Model a ~ a,        EqTest a ~ Property

testSanity :: forall v a. (COMMON_CONTEXT(a)) => S.Bundle v a -> [Test]
testSanity _ = [
        testProperty "fromList.toList == id" prop_fromList_toList,
        testProperty "toList.fromList == id" prop_toList_fromList
    ]
  where
    prop_fromList_toList :: P (S.Bundle v a -> S.Bundle v a)
        = (S.fromList . S.toList) `eq` id
    prop_toList_fromList :: P ([a] -> [a])
        = (S.toList . (S.fromList :: [a] -> S.Bundle v a)) `eq` id

testPolymorphicFunctions :: forall v a. (COMMON_CONTEXT(a)) => S.Bundle v a -> [Test]
testPolymorphicFunctions _ = $(testProperties [
        'prop_eq,

        'prop_length, 'prop_null,

        'prop_empty, 'prop_singleton, 'prop_replicate,
        'prop_cons, 'prop_snoc, 'prop_append,

        'prop_head, 'prop_last, 'prop_index,

        'prop_extract, 'prop_init, 'prop_tail, 'prop_take, 'prop_drop,

        'prop_map, 'prop_zipWith, 'prop_zipWith3,
        'prop_filter, 'prop_takeWhile, 'prop_dropWhile,

        'prop_elem, 'prop_notElem,
        'prop_find, 'prop_findIndex,

        'prop_foldl, 'prop_foldl1, 'prop_foldl', 'prop_foldl1',
        'prop_foldr, 'prop_foldr1,

        'prop_prescanl, 'prop_prescanl',
        'prop_postscanl, 'prop_postscanl',
        'prop_scanl, 'prop_scanl', 'prop_scanl1, 'prop_scanl1',

        'prop_concatMap,
        'prop_unfoldr
    ])
  where
    -- Prelude
    prop_eq :: P (S.Bundle v a -> S.Bundle v a -> Bool) = (==) `eq` (==)

    prop_length :: P (S.Bundle v a -> Int)     = S.length `eq` length
    prop_null   :: P (S.Bundle v a -> Bool)    = S.null `eq` null
    prop_empty  :: P (S.Bundle v a)            = S.empty `eq` []
    prop_singleton :: P (a -> S.Bundle v a)    = S.singleton `eq` singleton
    prop_replicate :: P (Int -> a -> S.Bundle v a)
              = (\n _ -> n < 1000) ===> S.replicate `eq` replicate
    prop_cons      :: P (a -> S.Bundle v a -> S.Bundle v a) = S.cons `eq` (:)
    prop_snoc      :: P (S.Bundle v a -> a -> S.Bundle v a) = S.snoc `eq` snoc
    prop_append    :: P (S.Bundle v a -> S.Bundle v a -> S.Bundle v a) = (S.++) `eq` (++)

    prop_head      :: P (S.Bundle v a -> a) = not . S.null ===> S.head `eq` head
    prop_last      :: P (S.Bundle v a -> a) = not . S.null ===> S.last `eq` last
    prop_index        = \xs ->
                        not (S.null xs) ==>
                        forAll (choose (0, S.length xs-1)) $ \i ->
                        unP prop xs i
      where
        prop :: P (S.Bundle v a -> Int -> a) = (S.!!) `eq` (!!)

    prop_extract      = \xs ->
                        forAll (choose (0, S.length xs))     $ \i ->
                        forAll (choose (0, S.length xs - i)) $ \n ->
                        unP prop i n xs
      where
        prop :: P (Int -> Int -> S.Bundle v a -> S.Bundle v a) = S.slice `eq` slice

    prop_tail :: P (S.Bundle v a -> S.Bundle v a) = not . S.null ===> S.tail `eq` tail
    prop_init :: P (S.Bundle v a -> S.Bundle v a) = not . S.null ===> S.init `eq` init
    prop_take :: P (Int -> S.Bundle v a -> S.Bundle v a) = S.take `eq` take
    prop_drop :: P (Int -> S.Bundle v a -> S.Bundle v a) = S.drop `eq` drop

    prop_map :: P ((a -> a) -> S.Bundle v a -> S.Bundle v a) = S.map `eq` map
    prop_zipWith :: P ((a -> a -> a) -> S.Bundle v a -> S.Bundle v a -> S.Bundle v a) = S.zipWith `eq` zipWith
    prop_zipWith3 :: P ((a -> a -> a -> a) -> S.Bundle v a -> S.Bundle v a -> S.Bundle v a -> S.Bundle v a)
             = S.zipWith3 `eq` zipWith3

    prop_filter :: P ((a -> Bool) -> S.Bundle v a -> S.Bundle v a) = S.filter `eq` filter
    prop_takeWhile :: P ((a -> Bool) -> S.Bundle v a -> S.Bundle v a) = S.takeWhile `eq` takeWhile
    prop_dropWhile :: P ((a -> Bool) -> S.Bundle v a -> S.Bundle v a) = S.dropWhile `eq` dropWhile

    prop_elem    :: P (a -> S.Bundle v a -> Bool) = S.elem `eq` elem
    prop_notElem :: P (a -> S.Bundle v a -> Bool) = S.notElem `eq` notElem
    prop_find    :: P ((a -> Bool) -> S.Bundle v a -> Maybe a) = S.find `eq` find
    prop_findIndex :: P ((a -> Bool) -> S.Bundle v a -> Maybe Int)
      = S.findIndex `eq` findIndex

    prop_foldl :: P ((a -> a -> a) -> a -> S.Bundle v a -> a) = S.foldl `eq` foldl
    prop_foldl1 :: P ((a -> a -> a) -> S.Bundle v a -> a)     = notNullS2 ===>
                        S.foldl1 `eq` foldl1
    prop_foldl' :: P ((a -> a -> a) -> a -> S.Bundle v a -> a) = S.foldl' `eq` foldl'
    prop_foldl1' :: P ((a -> a -> a) -> S.Bundle v a -> a)     = notNullS2 ===>
                        S.foldl1' `eq` foldl1'
    prop_foldr :: P ((a -> a -> a) -> a -> S.Bundle v a -> a) = S.foldr `eq` foldr
    prop_foldr1 :: P ((a -> a -> a) -> S.Bundle v a -> a)     = notNullS2 ===>
                        S.foldr1 `eq` foldr1

    prop_prescanl :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
                = S.prescanl `eq` prescanl
    prop_prescanl' :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
                = S.prescanl' `eq` prescanl
    prop_postscanl :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
                = S.postscanl `eq` postscanl
    prop_postscanl' :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
                = S.postscanl' `eq` postscanl
    prop_scanl :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
                = S.scanl `eq` scanl
    prop_scanl' :: P ((a -> a -> a) -> a -> S.Bundle v a -> S.Bundle v a)
               = S.scanl' `eq` scanl
    prop_scanl1 :: P ((a -> a -> a) -> S.Bundle v a -> S.Bundle v a) = notNullS2 ===>
                 S.scanl1 `eq` scanl1
    prop_scanl1' :: P ((a -> a -> a) -> S.Bundle v a -> S.Bundle v a) = notNullS2 ===>
                 S.scanl1' `eq` scanl1
 
    prop_concatMap    = forAll arbitrary $ \xs ->
                        forAll (sized (\n -> resize (n `div` S.length xs) arbitrary)) $ \f -> unP prop f xs
      where
        prop :: P ((a -> S.Bundle v a) -> S.Bundle v a -> S.Bundle v a) = S.concatMap `eq` concatMap

    limitUnfolds f (theirs, ours) | ours >= 0
                                  , Just (out, theirs') <- f theirs = Just (out, (theirs', ours - 1))
                                  | otherwise                       = Nothing
    prop_unfoldr :: P (Int -> (Int -> Maybe (a,Int)) -> Int -> S.Bundle v a)
         = (\n f a -> S.unfoldr (limitUnfolds f) (a, n))
           `eq` (\n f a -> unfoldr (limitUnfolds f) (a, n))

testBoolFunctions :: forall v. S.Bundle v Bool -> [Test]
testBoolFunctions _ = $(testProperties ['prop_and, 'prop_or ])
  where
    prop_and :: P (S.Bundle v Bool -> Bool) = S.and `eq` and
    prop_or  :: P (S.Bundle v Bool -> Bool) = S.or `eq` or

testBundleFunctions = testSanity (undefined :: S.Bundle v Int)
                      ++ testPolymorphicFunctions (undefined :: S.Bundle v Int)
                      ++ testBoolFunctions (undefined :: S.Bundle v Bool)

tests = [ testGroup "Data.Vector.Fusion.Bundle" testBundleFunctions ]

