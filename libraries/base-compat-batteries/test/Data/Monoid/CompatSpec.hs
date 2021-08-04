module Data.Monoid.CompatSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Monoid.Compat

import           Prelude ()
import           Prelude.Compat (IO, String, ($))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "<>" $ do
    it "is an infix synonym for mappend" $ do
      property $ \xs ys -> do
        xs <> ys `shouldBe` (mappend xs ys :: String)
