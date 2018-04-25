{-# LANGUAGE OverloadedStrings #-}
module Documentation.Haddock.Parser.UtilSpec (main, spec) where

import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Parser.Util
import Data.Either.Compat (isLeft)
import Test.Hspec
import Control.Applicative

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
