module Data.Bool.CompatSpec (main, spec) where

import           Test.Hspec

import           Data.Bool.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bool" $ do
    it "evaluates to first parameter if condition is False" $ do
      bool "KO" "OK" False `shouldBe` "KO"

    it "evaluates to second parameter if condition is True" $ do
      bool "KO" "OK" True `shouldBe` "OK"

