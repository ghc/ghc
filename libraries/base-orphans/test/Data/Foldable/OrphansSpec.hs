module Data.Foldable.OrphansSpec (main, spec) where

import Test.Hspec

import Control.Applicative
import Data.Foldable as F
import Data.Monoid
import Data.Orphans ()
import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Either Foldable Instance" $ do
    it "foldMap returns mempty for a Left value" $
      foldMap (`mappend` "+") (Left "abc" :: Either String String) `shouldBe` mempty
    it "foldMap returns the result of the function on the Right value" $
      foldMap (`mappend` "+") (Right "abc" :: Either String String) `shouldBe` "abc+"

    it "foldr returns the accumulator for a Left value" $
      F.foldr mappend "+" (Left "abc" :: Either String String) `shouldBe` "+"
    it "foldr returns the result of the function on the Right value and accumulator" $
      F.foldr mappend "+" (Right "abc" :: Either String String) `shouldBe` "abc+"

  describe "(,) Foldable Instance" $ do
    it "foldMap returns the result of the function applied to the second element" $
      foldMap (`mappend` "+") ("xyz","abc") `shouldBe` "abc+"

    it "foldr returns the result of the function on the second element of the tuple and accumulator" $
      F.foldr mappend "+" ("xyz","abc") `shouldBe` "abc+"

  describe "Const m Foldable Instance" $ do
    it "foldMap always returns mempty" $
      foldMap (`mappend` "+") (Const "abc") `shouldBe` ""
    it "foldr applies the function to the accumulator and mempty" $ do
      F.foldr mappend "+" (Const "abc") `shouldBe` "+"
