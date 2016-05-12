{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rules.Selftest (selftestRules) where

import Development.Shake
import Test.QuickCheck

import Base
import Oracles.ModuleFiles (decodeModule, encodeModule)
import Settings.Builders.Ar (chunksOfSize)
import Way

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

test :: Testable a => a -> Action ()
test = liftIO . quickCheck

selftestRules :: Rules ()
selftestRules =
    "selftest" ~> do
        testWays
        testChunksOfSize
        testMatchVersionedFilePath
        testModuleNames
        testLookupAll

testWays :: Action ()
testWays = do
    putBuild $ "==== Read Way, Show Way"
    test $ \(x :: Way) -> read (show x) == x

testChunksOfSize :: Action ()
testChunksOfSize = do
    putBuild $ "==== chunksOfSize"
    test $ chunksOfSize 3 [  "a", "b", "c" ,  "defg" ,  "hi" ,  "jk"  ]
                       == [ ["a", "b", "c"], ["defg"], ["hi"], ["jk"] ]
    test $ \n xs ->
        let res = chunksOfSize n xs
        in concat res == xs && all (\r -> length r == 1 || length (concat r) <= n) res

testMatchVersionedFilePath :: Action ()
testMatchVersionedFilePath = do
    putBuild $ "==== matchVersionedFilePath"
    test $ matchVersionedFilePath "foo/bar"  ".a" "foo/bar.a"     == True
    test $ matchVersionedFilePath "foo/bar"  ".a" "foo\\bar.a"    == False
    test $ matchVersionedFilePath "foo/bar"  "a"  "foo/bar.a"     == True
    test $ matchVersionedFilePath "foo/bar"  ""   "foo/bar.a"     == False
    test $ matchVersionedFilePath "foo/bar"  "a"  "foo/bar-0.1.a" == True
    test $ matchVersionedFilePath "foo/bar-" "a"  "foo/bar-0.1.a" == True
    test $ matchVersionedFilePath "foo/bar/" "a"  "foo/bar-0.1.a" == False

    test $ \prefix suffix -> forAll versions $ \version ->
        matchVersionedFilePath prefix suffix (prefix ++ version ++ suffix)
  where
    versions = listOf . elements $ '-' : '.' : ['0'..'9']

testModuleNames :: Action ()
testModuleNames = do
    putBuild $ "==== Encode/decode module name"
    test $ encodeModule "Data/Functor" "Identity.hs" == "Data.Functor.Identity"
    test $ encodeModule "" "Prelude"                 == "Prelude"

    test $ decodeModule "Data.Functor.Identity" == ("Data/Functor", "Identity")
    test $ decodeModule "Prelude"               == ("", "Prelude")

    test $ forAll names $ \n -> uncurry encodeModule (decodeModule n) == n
  where
    names = intercalate "." <$> listOf1 (listOf1 $ elements "abcABC123_'")

testLookupAll :: Action ()
testLookupAll = do
    putBuild $ "==== lookupAll"
    test $ lookupAll ["b"    , "c"            ] [("a", 1), ("c", 3), ("d", 4)]
                  == [Nothing, Just (3 :: Int)]
    test $ forAll dicts $ \dict -> forAll extras $ \extra ->
        let items = sort $ map fst dict ++ extra
        in lookupAll items (sort dict) == map (flip lookup dict) items
  where
    dicts :: Gen [(Int, Int)]
    dicts = nubBy ((==) `on` fst) <$> vector 20
    extras :: Gen [Int]
    extras = vector 20
