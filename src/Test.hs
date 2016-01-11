{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test (testRules) where

import Way
import Development.Shake
import Test.QuickCheck
import Settings.Builders.Ar(chunksOfSize)

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

testRules :: Rules ()
testRules =
    phony "selftest" $ do
        test $ \(x :: Way) -> read (show x) == x
        test $ \n xs ->
            let res = chunksOfSize n xs
            in concat res == xs && all (\r -> length r == 1 || length (concat r) <= n) res
        test $ chunksOfSize 3 ["a","b","c","defg","hi","jk"] == [["a","b","c"],["defg"],["hi"],["jk"]]


test :: Testable a => a -> Action ()
test = liftIO . quickCheck
