module Foreign.Storable.OrphansSpec (main, spec) where

import Test.Hspec
import Data.Complex
import Data.Orphans ()
import Data.Ratio
import Foreign.Storable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Storable Complex instance" $ do
    it "has twice the sizeOf its realPart" $ do
      sizeOf (undefined :: Complex Double) `shouldBe` 2*sizeOf (1 :: Double)
    it "has the alignment of its realPart" $ do
      alignment (undefined :: Complex Double) `shouldBe` alignment (1 :: Double)

  describe "Storable Ratio instance" $ do
    it "has twice the sizeOf its parameterized type" $ do
      sizeOf (undefined :: Ratio Int) `shouldBe` 2*sizeOf (1 :: Int)
    it "has the alignment of its parameterized type" $ do
      alignment (undefined :: Ratio Int) `shouldBe` alignment (1 :: Int)
