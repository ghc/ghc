{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rules.Selftest (selftestRules) where

import Hadrian.Haskell.Cabal
import Test.QuickCheck

import Base
import Context
import Oracles.ModuleFiles
import Packages
import Settings
import Target
import Utilities

import qualified System.FilePath.Posix as Posix ((</>))

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
        testDependencies
        testLookupAll
        testModuleName
        testPackages
        testPaths
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

testDependencies :: Action ()
testDependencies = do
    putBuild "==== pkgDependencies"
    let pkgs = ghcPackages \\ [libffi] -- @libffi@ does not have a Cabal file.
    depLists <- mapM pkgDependencies pkgs
    test $ and [ deps == sort deps | deps <- depLists ]
    putBuild "==== Dependencies of the 'ghc-bin' binary"
    ghcDeps <- pkgDependencies ghc
    test $ pkgName compiler `elem` ghcDeps
    stage0Deps <- contextDependencies (vanillaContext Stage0 ghc)
    stage1Deps <- contextDependencies (vanillaContext Stage1 ghc)
    stage2Deps <- contextDependencies (vanillaContext Stage2 ghc)
    test $ vanillaContext Stage0 compiler `notElem` stage1Deps
    test $ vanillaContext Stage1 compiler `elem`    stage1Deps
    test $ vanillaContext Stage2 compiler `notElem` stage1Deps
    test $ stage1Deps /= stage0Deps
    test $ stage1Deps == stage2Deps

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
    putBuild "==== Packages, interpretInContext, configuration flags"
    forM_ [Stage0 ..] $ \stage -> do
        pkgs <- stagePackages stage
        when (win32 `elem` pkgs) . test $ windowsHost
        when (unix  `elem` pkgs) . test $ not windowsHost
        test $ pkgs == nubOrd pkgs

testWay :: Action ()
testWay = do
    putBuild "==== Read Way, Show Way"
    test $ \(x :: Way) -> read (show x) == x

testPaths :: Action ()
testPaths = do
    putBuild "==== Absolute, Relative Path Concatenation"
    test $ forAll paths $ \(path1, path2) ->
      path1 -/- path2 == path1 Posix.</> path2
  where
    paths = (,) <$> path <*> path
    path = frequency [(1, relativePath), (1, absolutePath)]
    relativePath = intercalate "/" <$> listOf1 (elements ["a"])
    absolutePath = ('/':) <$> relativePath
