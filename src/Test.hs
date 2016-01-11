{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test (testRules) where

import Way
import Development.Shake
import Test.QuickCheck

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

testRules :: Rules ()
testRules =
    phony "selftest" $ do
        test $ \(x :: Way) -> read (show x) == x


test :: Testable a => a -> Action ()
test = liftIO . quickCheck
