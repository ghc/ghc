{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module SizedSeq
  ( SizedSeq(..)
  , emptySS
  , addToSS
  , addListToSS
  , ssElts
  , sizeSS
  ) where

import Data.Binary
import Data.List
import GHC.Generics

data SizedSeq a = SizedSeq !Word [a]
  deriving (Generic, Show)

instance Functor SizedSeq where
  fmap f (SizedSeq sz l) = SizedSeq sz (fmap f l)

instance Foldable SizedSeq where
  foldr f c ss = foldr f c (ssElts ss)

instance Traversable SizedSeq where
  traverse f (SizedSeq sz l) = SizedSeq sz . reverse <$> traverse f (reverse l)

instance Binary a => Binary (SizedSeq a)

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
