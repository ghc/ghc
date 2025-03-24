-- Here -fno-specialise-incoherents is in effect, but f refers unambiguously to
-- a single instance (because there are no others), so it should be specialised.

{-# OPTIONS_GHC -fno-specialise-incoherents #-}
{-# LANGUAGE UndecidableInstances #-}
module T25833c (y) where

class C a where
  m :: a -> a

instance {-# INCOHERENT #-} Num a => C a where
  m = (* 3)

f :: Num a => a -> a
f = m

y :: Int
y = f 2
