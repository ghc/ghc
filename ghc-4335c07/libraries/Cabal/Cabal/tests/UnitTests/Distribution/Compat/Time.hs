module UnitTests.Distribution.Compat.Time (tests) where

import Control.Concurrent (threadDelay)
import System.FilePath

import Distribution.Simple.Utils (withTempDirectory)
import Distribution.Verbosity

import Distribution.Compat.Time

import Test.Tasty
import Test.Tasty.HUnit

tests :: Int -> [TestTree]
tests mtimeChange =
  [ testCase "getModTime has sub-second resolution" $ getModTimeTest mtimeChange
  , testCase "getCurTime works as expected"         $ getCurTimeTest mtimeChange
  ]

getModTimeTest :: Int -> Assertion
getModTimeTest mtimeChange =
  withTempDirectory silent "." "getmodtime-" $ \dir -> do
    let fileName = dir </> "foo"
    writeFile fileName "bar"
    t0 <- getModTime fileName
    threadDelay mtimeChange
    writeFile fileName "baz"
    t1 <- getModTime fileName
    assertBool "expected different file mtimes" (t1 > t0)


getCurTimeTest :: Int -> Assertion
getCurTimeTest mtimeChange =
  withTempDirectory silent "." "getmodtime-" $ \dir -> do
    let fileName = dir </> "foo"
    writeFile fileName "bar"
    t0 <- getModTime fileName
    threadDelay mtimeChange
    t1 <- getCurTime
    assertBool("expected file mtime (" ++ show t0
               ++ ") to be earlier than current time (" ++ show t1 ++ ")")
      (t0 < t1)

    threadDelay mtimeChange
    writeFile fileName "baz"
    t2 <- getModTime fileName
    assertBool ("expected current time (" ++ show t1
                ++ ") to be earlier than file mtime (" ++ show t2 ++ ")")
      (t1 < t2)
