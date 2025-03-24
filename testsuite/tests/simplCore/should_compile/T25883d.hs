{-# OPTIONS_GHC -fno-specialise-incoherents #-}

module T25883d (y) where

import T25883d_import

instance {-# INCOHERENT #-} C () where
  m = id

f :: Num a => a -> a
f = m

y :: Int
y = f 2