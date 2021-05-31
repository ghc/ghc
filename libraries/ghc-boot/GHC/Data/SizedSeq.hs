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

-- | SizedSeq
--
-- The size is strict but the list of elements is lazy.
data SizedSeq a = SizedSeq {-# UNPACK #-} !Word [a]
  deriving (Generic, Show)

instance Functor SizedSeq where
  fmap f (SizedSeq sz l) = SizedSeq sz (fmap f l)

instance Foldable SizedSeq where
  foldr f c ss = foldr f c (ssElts ss)

instance Traversable SizedSeq where
  traverse f (SizedSeq sz l) = SizedSeq sz <$> traverse f l

instance Binary a => Binary (SizedSeq a)

instance NFData a => NFData (SizedSeq a) where
  rnf (SizedSeq _ xs) = rnf xs

emptySS :: SizedSeq a
emptySS = SizedSeq 0 []

addToSS :: SizedSeq a -> a -> SizedSeq a
addToSS (SizedSeq n l) x = SizedSeq (n+1) (l ++ [x])

addListToSS :: SizedSeq a -> [a] -> SizedSeq a
addListToSS (SizedSeq n xs) ys = SizedSeq (n + genericLength ys) (xs ++ ys)

ssElts :: SizedSeq a -> [a]
ssElts (SizedSeq _ l) = l

sizeSS :: SizedSeq a -> Word
sizeSS (SizedSeq n _) = n
