{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rules.Selftest (selftestRules) where

import Development.Shake
import Test.QuickCheck

import Base
import Settings.Builders.Ar (chunksOfSize)
import Way

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

selftestRules :: Rules ()
selftestRules =
    "selftest" ~> do
        test $ \(x :: Way) -> read (show x) == x
        test $ \n xs ->
            let res = chunksOfSize n xs
            in concat res == xs && all (\r -> length r == 1 || length (concat r) <= n) res
        test $ chunksOfSize 3 ["a","b","c","defg","hi","jk"] == [["a","b","c"],["defg"],["hi"],["jk"]]

        test $ matchVersionedFilePath "foo/bar"  ".a" "foo/bar.a"     == True
        test $ matchVersionedFilePath "foo/bar"  ".a" "foo\\bar.a"    == True
        test $ matchVersionedFilePath "foo/bar"  "a"  "foo/bar.a"     == True
        test $ matchVersionedFilePath "foo/bar"  ""   "foo/bar.a"     == False
        test $ matchVersionedFilePath "foo/bar"  "a"  "foo/bar-0.1.a" == True
        test $ matchVersionedFilePath "foo/bar-" "a"  "foo/bar-0.1.a" == True
        test $ matchVersionedFilePath "foo/bar/" "a"  "foo/bar-0.1.a" == False

        -- TODO: add automated tests for matchVersionedFilePath too

test :: Testable a => a -> Action ()
test = liftIO . quickCheck
