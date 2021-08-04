module Data.Either.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Either.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isLeft" $ do
    it "returns True for a Left value" $ do
      isLeft (Left "23" :: Either String String) `shouldBe` True

    it "returns False for a Right value" $ do
      isLeft (Right "23" :: Either String String) `shouldBe` False

  describe "isRight" $ do
    it "returns False for a Left value" $ do
      isRight (Left "23" :: Either String String) `shouldBe` False

    it "returns True for a Right value" $ do
      isRight (Right "23" :: Either String String) `shouldBe` True

