{-# LANGUAGE CPP #-}
module Main where

#if ! MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import           System.Directory          (getTemporaryDirectory)
import           System.FilePath           ((</>))
import           Test.HUnit

import           Distribution.Simple.Utils (withTempDirectory)
import           Distribution.Verbosity    (silent)

import           Data.Binary

data Foo = Bar !Word32 !Word32 !Word32 deriving (Eq, Show)

instance Binary Foo where
  get = Bar <$> get <*> get <*> get
  put (Bar a b c) = put (a,b,c)

exampleData :: [Foo]
exampleData = make bytes
  where
    make (a:b:c:xs) = Bar a b c : make xs
    make _ = []
    bytes = take (256*1024) (cycle [minBound..maxBound])

readWriteTest :: Test
readWriteTest = TestCase $ do
  tmpDir <- getTemporaryDirectory
  withTempDirectory silent tmpDir "foo-dir" $ \dir -> do
    let fn = dir </> "foo.bin"
    encodeFile fn exampleData
    content <- decodeFile fn
    -- It'd be nice to use lsof to verify that 'fn' isn't still open.
    exampleData @=? content

main :: IO ()
main = do 
  _ <- runTestTT readWriteTest
  return ()