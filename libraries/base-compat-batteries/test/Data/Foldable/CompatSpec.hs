module Data.Foldable.CompatSpec (main, spec) where

import            Test.Hspec
import            Data.Foldable.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "maximumBy" $ do
    it "runs in constant space" $ do
      maximumBy compare [1..10000] `shouldBe` (10000 :: Int)
