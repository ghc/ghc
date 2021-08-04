module Text.Read.CompatSpec (main, spec) where

import           Test.Hspec

import           Text.Read.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readMaybe" $ do
    it "parses a value" $ do
      readMaybe "23" `shouldBe` (Just 23 :: Maybe Int)

    it "returns Nothing if parsing fails" $ do
      readMaybe "xx" `shouldBe` (Nothing :: Maybe Int)

  describe "readEither" $ do
    it "parses a value" $ do
      readEither "23" `shouldBe` (Right 23 :: Either String Int)

    it "returns Left if parsing fails" $ do
      readEither "xx" `shouldBe` (Left "Prelude.read: no parse" :: Either String Int)
