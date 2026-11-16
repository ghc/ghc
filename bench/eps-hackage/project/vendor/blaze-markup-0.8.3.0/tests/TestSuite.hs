-- | Main module to run all tests.
--
module Main where

import qualified Test.Tasty as Tasty

import qualified Text.Blaze.Tests

main :: IO ()
main = Tasty.defaultMain $
    Tasty.testGroup "Text.Blaze.Tests" Text.Blaze.Tests.tests
