module Data.Traversable.OrphansSpec (main, spec) where

import Test.Hspec

import Control.Applicative
import Data.Orphans ()
import Data.Traversable
import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Either Traversable Instance" $ do
    it "traverses a Left value" $
      traverse (:[]) (Left 5 :: Either Int String) `shouldBe` [Left 5]
    it "traverses a Right Value" $
      traverse (:[]) (Right "aaa" :: Either Int String) `shouldBe` [Right "aaa"]

  describe "(,) a Traversable Instance" $ do
    it "traverses a (,) a value" $
      traverse (:[]) (5::Int,"aaa") `shouldBe` [(5,"aaa")]

  describe "Const m Traversable Instance" $ do
    it "traverses a Const a value" $ do
      fmap getConst (traverse (:[]) (Const 5 :: Const Int String)) `shouldBe` [5]
