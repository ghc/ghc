{-# LANGUAGE CPP #-}
module Main where

import qualified Data.ByteString.Builder.Tests
import qualified Data.ByteString.Builder.Prim.Tests
#if defined(HAVE_TEST_FRAMEWORK)
import           Test.Framework (defaultMain, Test, testGroup)
#else
import           TestFramework
#endif

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Data.ByteString.Builder"
       Data.ByteString.Builder.Tests.tests

  , testGroup "Data.ByteString.Lazy.Builder.BasicEncoding"
       Data.ByteString.Builder.Prim.Tests.tests
  ]
