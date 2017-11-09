{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rules.Selftest (selftestRules) where

import Test.QuickCheck

import Base
import GHC
import Oracles.ModuleFiles
import Oracles.Setting
import Settings
import Target

instance Arbitrary Way where
    arbitrary = wayFromUnits <$> arbitrary

instance Arbitrary WayUnit where
    arbitrary = arbitraryBoundedEnum

test :: Testable a => a -> Action ()
test = liftIO . quickCheck

selftestRules :: Rules ()
selftestRules =
    "selftest" ~> do
        testBuilder
        testChunksOfSize
        testLookupAll
        testModuleName
        testPackages
        testWay

testBuilder :: Action ()
testBuilder = do
    putBuild "==== trackArgument"
    let make = target undefined (Make undefined) undefined undefined
    test $ forAll (elements ["-j", "MAKEFLAGS=-j", "THREADS="])
         $ \prefix (NonNegative n) ->
            not (trackArgument make prefix) &&
            not (trackArgument make ("-j" ++ show (n :: Int)))

testChunksOfSize :: Action ()
testChunksOfSize = do
    putBuild "==== chunksOfSize"
    test $ chunksOfSize 3 [  "a", "b", "c" ,  "defg" ,  "hi" ,  "jk"  ]
                       == [ ["a", "b", "c"], ["defg"], ["hi"], ["jk"] ]
    test $ \n xs ->
        let res = chunksOfSize n xs
        in concat res == xs && all (\r -> length r == 1 || length (concat r) <= n) res

testLookupAll :: Action ()
testLookupAll = do
    putBuild "==== lookupAll"
    test $ lookupAll ["b"    , "c"            ] [("a", 1), ("c", 3), ("d", 4)]
                  == [Nothing, Just (3 :: Int)]
    test $ forAll dicts $ \dict -> forAll extras $ \extra ->
        let items = sort $ map fst dict ++ extra
        in lookupAll items (sort dict) == map (`lookup` dict) items
  where
    dicts :: Gen [(Int, Int)]
    dicts = nubBy (\x y -> fst x == fst y) <$> vector 20
    extras :: Gen [Int]
    extras = vector 20

testModuleName :: Action ()
testModuleName = do
    putBuild "==== Encode/decode module name"
    test $ encodeModule "Data/Functor" "Identity.hs" == "Data.Functor.Identity"
    test $ encodeModule "" "Prelude"                 == "Prelude"

    test $ decodeModule "Data.Functor.Identity" == ("Data/Functor", "Identity")
    test $ decodeModule "Prelude"               == ("", "Prelude")

    test $ forAll names $ \n -> uncurry encodeModule (decodeModule n) == n
  where
    names = intercalate "." <$> listOf1 (listOf1 $ elements "abcABC123_'")

testPackages :: Action ()
testPackages = do
    putBuild "==== Check system configuration"
    win <- windowsHost -- This depends on the @boot@ and @configure@ scripts.
    putBuild "==== Packages, interpretInContext, configuration flags"
    forM_ [Stage0 ..] $ \stage -> do
        pkgs <- stagePackages stage
        when (win32 `elem` pkgs) . test $ win
        when (unix  `elem` pkgs) . test $ not win
        test $ pkgs == nubOrd pkgs

testWay :: Action ()
testWay = do
    putBuild "==== Read Way, Show Way"
    test $ \(x :: Way) -> read (show x) == x

