module Main where

import           Test.Hspec

import qualified Haddock.ParseSpec

main :: IO ()
main = hspec $ do
  describe "Haddock.Parse" Haddock.ParseSpec.spec
