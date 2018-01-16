-- to suppress WARNING in "Distribution.Compat.Prelude.Internal"
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module UnitTests.Distribution.Utils.NubList
    ( tests
    ) where

import Prelude ()
import Distribution.Compat.Prelude.Internal

import Distribution.Utils.NubList
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testCase "NubList retains ordering example" testOrdering
    , testCase "NubList removes duplicates example" testDeDupe
    , testProperty "NubList retains ordering" prop_Ordering
    , testProperty "NubList removes duplicates" prop_DeDupe
    , testProperty "fromNubList . toNubList = nub" prop_Nub
    , testProperty "Monoid NubList Identity" prop_Identity
    , testProperty "Monoid NubList Associativity" prop_Associativity
    -- NubListR
    , testProperty "NubListR removes duplicates from the right" prop_DeDupeR
    ]

someIntList :: [Int]
-- This list must not have duplicate entries.
someIntList = [ 1, 3, 4, 2, 0, 7, 6, 5, 9, -1 ]

testOrdering :: Assertion
testOrdering =
    assertBool "Maintains element ordering:" $
        fromNubList (toNubList someIntList) == someIntList

testDeDupe :: Assertion
testDeDupe =
    assertBool "De-duplicates a list:" $
        fromNubList (toNubList (someIntList ++ someIntList)) == someIntList

-- ---------------------------------------------------------------------------
-- QuickCheck properties for NubList

prop_Ordering :: [Int] -> Property
prop_Ordering xs =
    mempty <> toNubList xs' === toNubList xs' <> mempty
  where
    xs' = nub xs

prop_DeDupe :: [Int] -> Property
prop_DeDupe xs =
    fromNubList (toNubList (xs' ++ xs)) === xs' -- Note, we append primeless xs
  where
    xs' = nub xs

prop_DeDupeR :: [Int] -> Property
prop_DeDupeR xs =
    fromNubListR (toNubListR (xs ++ xs')) === xs' -- Note, we prepend primeless xs
  where
    xs' = nub xs

prop_Nub :: [Int] -> Property
prop_Nub xs = rhs === lhs
  where
    rhs = fromNubList (toNubList xs)
    lhs = nub xs

prop_Identity :: [Int] -> Bool
prop_Identity xs =
    mempty `mappend` toNubList xs == toNubList xs `mappend` mempty

prop_Associativity :: [Int] -> [Int] -> [Int] -> Bool
prop_Associativity xs ys zs =
    (toNubList xs `mappend` toNubList ys) `mappend` toNubList zs
            == toNubList xs `mappend` (toNubList ys `mappend` toNubList zs)
