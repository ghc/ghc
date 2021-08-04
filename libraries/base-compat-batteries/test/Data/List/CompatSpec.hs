module Data.List.CompatSpec (main, spec) where

import           Test.Hspec
import           Data.List.Compat

data Asymmetric = A | B deriving Show

instance Eq Asymmetric where
  A == _ = True
  B == _ = False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dropWhileEnd" $ do
    it "drops the largest suffix of a list in which a predicate holds for all elements" $ do
      dropWhileEnd (== ' ') "foo "    `shouldBe` "foo"
      dropWhileEnd (== ' ') "foo bar" `shouldBe` "foo bar"
  describe "isSubsequenceOf" $ do
    it "returns True if the first list is a subsequence of the second list" $ do
      isSubsequenceOf "GHC" "The Glorious Haskell Compiler" `shouldBe` True
      isSubsequenceOf "JHC" "The Glorious Haskell Compiler" `shouldBe` False
  describe "nub" $
    it "preserves the order of arguments to (==)" $
      nub [A, B] `shouldBe` [A]
  describe "nubBy" $
    it "preserves the order of arguments to the equality function" $
      nubBy (<) "12" `shouldBe` "1"
  describe "sortOn" $ do
    it "sorts a list by comparing the results of a key function applied to each element" $ do
      sortOn (>='b') "cba" `shouldBe` "acb"
  describe "uncons" $ do
    it "decomposes a list into its head and tail" $ do
      uncons ""   `shouldBe` Nothing
      uncons "12" `shouldBe` Just ('1', "2")
  describe "union" $
    it "nubs arguments in the same order as (==)" $ do
      union [A] [A, B] `shouldBe` [A]
  describe "unionBy" $
    it "nubs arguments in the same order as nubBy's equality function" $ do
      unionBy (<) "1" "21" `shouldBe` "11"
