module Control.Monad.CompatSpec (main, spec) where

import           Test.Hspec

import           Control.Monad.Compat
import           Prelude ()
import           Prelude.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "(<$!>)" $ do
    it "is a strict version of (<$>)" $ do
      not <$!> [True, False] `shouldBe` not <$> [True, False]
