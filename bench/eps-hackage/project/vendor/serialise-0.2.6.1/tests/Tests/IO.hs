{-# LANGUAGE CPP #-}
module Tests.IO
  ( testTree -- :: TestTree
  ) where
import           System.FilePath
import           System.Directory  (removeFile)
import           Control.Exception (bracket)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Codec.Serialise

--------------------------------------------------------------------------------
-- Tests and properties

test_encodeAndDecodeFile :: Assertion
test_encodeAndDecodeFile =
  let path = ("tests" </> "io_test1.cbor")
  in withDeleteFile path $ do
    let val = Just True
    writeFileSerialise path val
    val' <- readFileDeserialise path :: IO (Maybe Bool)
    val @=? val'

--------------------------------------------------------------------------------
-- TestTree API

testTree :: TestTree
testTree = testGroup "IO tests"
  [ testCase "file encode/decode roundtrip" test_encodeAndDecodeFile
  ]

--------------------------------------------------------------------------------
-- Utilities

-- | Run an action, and be sure to delete the specified @'FilePath'@ when
-- finished.
withDeleteFile :: FilePath -> IO a -> IO a
withDeleteFile f k = bracket (return ()) (const $ removeFile f) (const k)
