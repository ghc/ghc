{-# LANGUAGE CPP #-}
module Data.Bits.CompatSpec (main, spec) where

import Test.Hspec
import Data.Bits.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bitDefault" $
    it "sets the ith bit with all other bits clear" $ do
      bitDefault 0 `shouldBe` (1 :: Int)
      bitDefault 1 `shouldBe` (2 :: Int)
      bitDefault 2 `shouldBe` (4 :: Int)
      bitDefault 3 `shouldBe` (8 :: Int)
  describe "testBitDefault" $
    it "returns True if the nth bit of the argument is 1" $ do
      testBitDefault (10 :: Int) 0 `shouldBe` False
      testBitDefault (10 :: Int) 1 `shouldBe` True
      testBitDefault (10 :: Int) 2 `shouldBe` False
      testBitDefault (10 :: Int) 3 `shouldBe` True
  describe "popCountDefault" $
    it "returns the number of set bits in the argument" $ do
      popCountDefault (0  :: Int) `shouldBe` 0
      popCountDefault (1  :: Int) `shouldBe` 1
      popCountDefault (10 :: Int) `shouldBe` 2
#if MIN_VERSION_base(4,7,0)
  describe "toIntegralSized" $
    it "converts an Integral type to another as measured by bitSizeMaybe" $ do
      toIntegralSized (42 :: Integer)                   `shouldBe` (Just 42 :: Maybe Int)
      toIntegralSized (12345678901234567890 :: Integer) `shouldBe` (Nothing :: Maybe Int)
#endif
