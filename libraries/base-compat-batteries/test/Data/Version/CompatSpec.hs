module Data.Version.CompatSpec (spec) where

import           Test.Hspec
import           Data.Version.Compat

spec :: Spec
spec = do
  describe "makeVersion" $
    it "constructs a tagless Version" $
      makeVersion [1,2,3] `shouldBe` Version [1,2,3] []
