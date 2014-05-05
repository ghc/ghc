{-# LANGUAGE OverloadedStrings #-}
module Documentation.Haddock.Parser.UtilSpec (main, spec) where

import Data.Attoparsec.ByteString.Char8
import Data.Either
import Documentation.Haddock.Parser.Util
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "takeUntil" $ do
    it "takes everything until a specified byte sequence" $ do
      parseOnly (takeUntil "end") "someend" `shouldBe` Right "some"

    it "requires the end sequence" $ do
      parseOnly (takeUntil "end") "someen" `shouldSatisfy` isLeft

    it "takes escaped bytes unconditionally" $ do
      parseOnly (takeUntil "end") "some\\endend" `shouldBe` Right "some\\end"
