module UnitTests.Distribution.Client.Sandbox.Timestamp (tests) where

import System.FilePath

import Distribution.Simple.Utils (withTempDirectory)
import Distribution.Verbosity

import Distribution.Compat.Time
import Distribution.Client.Sandbox.Timestamp

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "timestamp record version 1 can be read" timestampReadTest_v1
  , testCase "timestamp record version 2 can be read" timestampReadTest_v2
  , testCase "written timestamp record can be read"   timestampReadWriteTest ]

timestampRecord_v1 :: String
timestampRecord_v1 =
  "[(\"i386-linux-ghc-8.0.0.20160204\",[(\"/foo/bar/Baz\",1455350946)])" ++
  ",(\"i386-linux-ghc-7.10.3\",[(\"/foo/bar/Baz\",1455484719)])]\n"

timestampRecord_v2 :: String
timestampRecord_v2 =
  "2\n" ++
  "[(\"i386-linux-ghc-8.0.0.20160204\",[(\"/foo/bar/Baz\",1455350946)])" ++
  ",(\"i386-linux-ghc-7.10.3\",[(\"/foo/bar/Baz\",1455484719)])]"

timestampReadTest_v1 :: Assertion
timestampReadTest_v1 =
  timestampReadTest timestampRecord_v1 $
  map (\(i, ts) ->
        (i, map (\(p, ModTime t) ->
                  (p, posixSecondsToModTime . fromIntegral $ t)) ts))
  timestampRecord

timestampReadTest_v2 :: Assertion
timestampReadTest_v2 = timestampReadTest timestampRecord_v2 timestampRecord

timestampReadTest :: FilePath -> [TimestampFileRecord] -> Assertion
timestampReadTest fileContent expected =
  withTempDirectory silent "." "cabal-timestamp-" $ \dir -> do
    let fileName = dir </> "timestamp-record"
    writeFile fileName fileContent
    tRec <- readTimestampFile normal fileName
    assertEqual "expected timestamp records to be equal"
      expected tRec

timestampRecord :: [TimestampFileRecord]
timestampRecord =
  [("i386-linux-ghc-8.0.0.20160204",[("/foo/bar/Baz",ModTime 1455350946)])
  ,("i386-linux-ghc-7.10.3",[("/foo/bar/Baz",ModTime 1455484719)])]

timestampReadWriteTest :: Assertion
timestampReadWriteTest =
  withTempDirectory silent "." "cabal-timestamp-" $ \dir -> do
    let fileName = dir </> "timestamp-record"
    writeTimestampFile fileName timestampRecord
    tRec <- readTimestampFile normal fileName
    assertEqual "expected timestamp records to be equal"
      timestampRecord tRec
