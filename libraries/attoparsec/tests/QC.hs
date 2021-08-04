{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified QC.Buffer as Buffer
import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import qualified QC.Simple as Simple
import qualified QC.Text as Text
import Test.Tasty (defaultMain, testGroup)

main = defaultMain tests

tests = testGroup "tests" [
    testGroup "bs" ByteString.tests
  , testGroup "buf" Buffer.tests
  , testGroup "combinator" Combinator.tests
  , testGroup "simple" Simple.tests
  , testGroup "text" Text.tests
  ]
