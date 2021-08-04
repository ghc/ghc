module Data.Monoid.OrphansSpec (main, spec) where

import Test.Hspec
import Data.Monoid
import Data.Orphans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Num (Sum a) instance" $
    it "allows a Sum value to be created from a number" $
      1 `shouldBe` Sum (1 :: Int)
  describe "Num (Product a) instance" $
    it "allows a Product value to be created from a number" $
      1 `shouldBe` Product (1 :: Int)
