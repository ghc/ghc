{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module GHC.Data.SizedSeq
  ( SizedSeq(..)
  , emptySS
  , addToSS
  , addListToSS
  , ssElts
  , sizeSS
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import Control.DeepSeq
import Data.Binary
import Data.List (genericLength)
import GHC.Generics

data SizedSeq a = SizedSeq {-# UNPACK #-} !Word [a]
  deriving (Generic, Show)

instance Functor SizedSeq where
  fmap f (SizedSeq sz l) = SizedSeq sz (fmap f l)

instance Foldable SizedSeq where
  foldr f c ss = foldr f c (ssElts ss)

instance Traversable SizedSeq where
  traverse f (SizedSeq sz l) = SizedSeq sz . reverse <$> traverse f (reverse l)

instance Binary a => Binary (SizedSeq a)

instance NFData a => NFData (SizedSeq a) where
  rnf (SizedSeq _ xs) = rnf xs

emptySS :: SizedSeq a
emptySS = SizedSeq 0 []

addToSS :: SizedSeq a -> a -> SizedSeq a
addToSS (SizedSeq n r_xs) x = SizedSeq (n+1) (x:r_xs)

addListToSS :: SizedSeq a -> [a] -> SizedSeq a
addListToSS (SizedSeq n r_xs) xs
  = SizedSeq (n + genericLength xs) (reverse xs ++ r_xs)

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq _ r_xs) = reverse r_xs

sizeSS :: SizedSeq a -> Word
sizeSS (SizedSeq n _) = n
