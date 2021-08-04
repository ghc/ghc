module Prelude.CompatSpec (main, spec) where

import           Test.Hspec

import           Prelude ()
import           Prelude.Compat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "($!)" $ do
    it "is infixr 0" $ do -- #54
      (succ $! succ $! 0) `shouldBe` (2 :: Int)
      (succ $! 2 *** 2)   `shouldBe` (5 :: Int)


infixr 1 ***
(***) :: Int -> Int -> Int
(***) = (*)
