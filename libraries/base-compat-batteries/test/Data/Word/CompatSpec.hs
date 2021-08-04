module Data.Word.CompatSpec (main, spec) where

import Test.Hspec
import Data.Word.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "byteSwap16" $
    it "reverses the order of bytes in a Word16 value" $ do
      byteSwap16 0x1100 `shouldBe` 0x0011
      byteSwap16 0x1010 `shouldBe` 0x1010
  describe "byteSwap32" $
    it "reverses the order of bytes in a Word32 value" $ do
      byteSwap32 0x11001010 `shouldBe` 0x10100011
      byteSwap32 0x10101111 `shouldBe` 0x11111010
  describe "byteSwap64" $
    it "reverses the order of bytes in a Word64 value" $ do
      byteSwap64 0x1010111110101111 `shouldBe` 0x1111101011111010
      byteSwap64 0x1100000000000011 `shouldBe` 0x1100000000000011
