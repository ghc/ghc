module Data.Functor.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Functor.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "void" $ do
    it "discards computation result" $ do
      void (return 1 :: IO Int) `shouldReturn` ()

  describe "$>" $ do
    it "is the same as flipped <$" $ do
      (Just 5 :: Maybe Int) $> 6 `shouldBe` (Just 6 :: Maybe Int)
