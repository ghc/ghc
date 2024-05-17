{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Documentation.Haddock.Parser.UtilSpec (main, spec) where

import Data.Either (isLeft)
import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Parser.Util
import Test.Hspec
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif

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
