module Data.Function.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.Function.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "&" $ do
    it "reverses function application" $ do
      (False & not) `shouldBe` True
