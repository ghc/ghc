{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.ChasingBottoms.IsBottom
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(arbitrary))
import Test.QuickCheck.Function (Fun(..), apply)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

instance (Arbitrary k, Arbitrary v, Ord k) =>
         Arbitrary (Map k v) where
    arbitrary = M.fromList `fmap` arbitrary

apply2 :: Fun (a, b) c -> a -> b -> c
apply2 f a b = apply f (a, b)

apply3 :: Fun (a, b, c) d -> a -> b -> c -> d
apply3 f a b c = apply f (a, b, c)

------------------------------------------------------------------------
-- * Properties

------------------------------------------------------------------------
-- ** Strict module

pSingletonKeyStrict :: Int -> Bool
pSingletonKeyStrict v = isBottom $ M.singleton (bottom :: Int) v

pSingletonValueStrict :: Int -> Bool
pSingletonValueStrict k = isBottom $ (M.singleton k (bottom :: Int))

pFindWithDefaultKeyStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultKeyStrict def m = isBottom $ M.findWithDefault def bottom m

pFindWithDefaultValueStrict :: Int -> Map Int Int -> Bool
pFindWithDefaultValueStrict k m =
    M.member k m || (isBottom $ M.findWithDefault bottom k m)

pAdjustKeyStrict :: Fun Int Int -> Map Int Int -> Bool
pAdjustKeyStrict f m = isBottom $ M.adjust (apply f) bottom m

pAdjustValueStrict :: Int -> Map Int Int -> Bool
pAdjustValueStrict k m
    | k `M.member` m = isBottom $ M.adjust (const bottom) k m
    | otherwise       = case M.keys m of
        []     -> True
        (k':_) -> isBottom $ M.adjust (const bottom) k' m

pInsertKeyStrict :: Int -> Map Int Int -> Bool
pInsertKeyStrict v m = isBottom $ M.insert bottom v m

pInsertValueStrict :: Int -> Map Int Int -> Bool
pInsertValueStrict k m = isBottom $ M.insert k bottom m

pInsertWithKeyStrict :: Fun (Int, Int) Int -> Int -> Map Int Int -> Bool
pInsertWithKeyStrict f v m = isBottom $ M.insertWith (apply2 f) bottom v m

pInsertWithValueStrict :: Fun (Int, Int) Int -> Int -> Int -> Map Int Int
                       -> Bool
pInsertWithValueStrict f k v m
    | M.member k m = (isBottom $ M.insertWith (const2 bottom) k v m) &&
                     not (isBottom $ M.insertWith (const2 1) k bottom m)
    | otherwise    = isBottom $ M.insertWith (apply2 f) k bottom m

pInsertLookupWithKeyKeyStrict :: Fun (Int, Int, Int) Int -> Int
                              -> Map Int Int -> Bool
pInsertLookupWithKeyKeyStrict f v m = isBottom $ M.insertLookupWithKey (apply3 f) bottom v m

pInsertLookupWithKeyValueStrict :: Fun (Int, Int, Int) Int -> Int -> Int
                                -> Map Int Int -> Bool
pInsertLookupWithKeyValueStrict f k v m
    | M.member k m = (isBottom $ M.insertLookupWithKey (const3 bottom) k v m) &&
                     not (isBottom $ M.insertLookupWithKey (const3 1) k bottom m)
    | otherwise    = isBottom $ M.insertLookupWithKey (apply3 f) k bottom m

------------------------------------------------------------------------
-- * Test list

tests :: [Test]
tests =
    [
    -- Basic interface
      testGroup "Map.Strict"
      [ testProperty "singleton is key-strict" pSingletonKeyStrict
      , testProperty "singleton is value-strict" pSingletonValueStrict
      , testProperty "member is key-strict" $ keyStrict M.member
      , testProperty "lookup is key-strict" $ keyStrict M.lookup
      , testProperty "findWithDefault is key-strict" pFindWithDefaultKeyStrict
      , testProperty "findWithDefault is value-strict" pFindWithDefaultValueStrict
      , testProperty "! is key-strict" $ keyStrict (flip (M.!))
      , testProperty "delete is key-strict" $ keyStrict M.delete
      , testProperty "adjust is key-strict" pAdjustKeyStrict
      , testProperty "adjust is value-strict" pAdjustValueStrict
      , testProperty "insert is key-strict" pInsertKeyStrict
      , testProperty "insert is value-strict" pInsertValueStrict
      , testProperty "insertWith is key-strict" pInsertWithKeyStrict
      , testProperty "insertWith is value-strict" pInsertWithValueStrict
      , testProperty "insertLookupWithKey is key-strict"
        pInsertLookupWithKeyKeyStrict
      , testProperty "insertLookupWithKey is value-strict"
        pInsertLookupWithKeyValueStrict
      ]
    ]

------------------------------------------------------------------------
-- * Test harness

main :: IO ()
main = defaultMain tests

------------------------------------------------------------------------
-- * Utilities

keyStrict :: (Int -> Map Int Int -> a) -> Map Int Int -> Bool
keyStrict f m = isBottom $ f bottom m

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x
