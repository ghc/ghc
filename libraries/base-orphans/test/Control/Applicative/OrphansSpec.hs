module Control.Applicative.OrphansSpec (main, spec) where

import Test.Hspec
import Control.Applicative
import Data.Orphans ()
import Data.Monoid
import Prelude

-- simplest one to use
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . runIdentity

instance Applicative Identity where
  pure     = Identity
  Identity f <*> x = f <$> x

instance Monad Identity where
  return = Identity
  m >>= k  = k (runIdentity m)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Monoid (Const a b)" $ do
    it "mempty returns an empty const" $
      getConst (mempty :: (Const String Int)) `shouldBe` ""
    it "mappends const part" $
      getConst ((Const "aaa" :: Const String Int) `mappend` (Const "bbb" :: Const String Int))
        `shouldBe` "aaabbb"

  describe "Monad (WrappedMonad m)" $
    it "allows to use a Monad interface in a WrappedMonad" $
      (runIdentity . unwrapMonad
        $  (WrapMonad (return 1 :: Identity Int))
        >> (WrapMonad (return 2 :: Identity Int)))
        `shouldBe` (2::Int)
