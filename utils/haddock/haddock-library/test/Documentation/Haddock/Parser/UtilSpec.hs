{-# LANGUAGE OverloadedStrings #-}

module Documentation.Haddock.Parser.UtilSpec (main, spec) where

import Data.Either (isLeft)
import Test.Hspec

import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Parser.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "takeUntil" $ do
    it "takes everything until a specified byte sequence" $ do
      snd <$> parseOnly (takeUntil "end") "someend" `shouldBe` Right "some"

    it "requires the end sequence" $ do
      snd <$> parseOnly (takeUntil "end") "someen" `shouldSatisfy` isLeft

    it "takes escaped bytes unconditionally" $ do
      snd <$> parseOnly (takeUntil "end") "some\\endend" `shouldBe` Right "some\\end"
