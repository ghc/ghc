module UnitTests.Distribution.Client.Sandbox (
  tests
  ) where

import Distribution.Client.Sandbox (withSandboxBinDirOnSearchPath)

import Test.Tasty
import Test.Tasty.HUnit

import System.FilePath             (getSearchPath, (</>))

tests :: [TestTree]
tests = [ testCase "sandboxBinDirOnSearchPath" sandboxBinDirOnSearchPathTest
        , testCase "oldSearchPathRestored" oldSearchPathRestoreTest
        ]

sandboxBinDirOnSearchPathTest :: Assertion
sandboxBinDirOnSearchPathTest =
  withSandboxBinDirOnSearchPath "foo" $ do
    r <- getSearchPath
    assertBool "'foo/bin' not on search path" $ ("foo" </> "bin") `elem` r

oldSearchPathRestoreTest :: Assertion
oldSearchPathRestoreTest = do
  r <- getSearchPath
  withSandboxBinDirOnSearchPath "foo" $ return ()
  r' <- getSearchPath
  assertEqual "Old search path wasn't restored" r r'
