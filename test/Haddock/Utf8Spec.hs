module Haddock.Utf8Spec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Haddock.Utf8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decodeUtf8" $ do
    it "is inverse to encodeUtf8" $ do
      property $ \xs -> (decodeUtf8 . encodeUtf8) xs `shouldBe` xs
