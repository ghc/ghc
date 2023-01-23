module RnMultipleMinimalPragmaFail where

class C a where
  {-# MINIMAL m1 | m2 #-}
  m1 :: a
  m2 :: a
  m2 = m1
  m1 = m2
  {-# MINIMAL m1 | m2 #-}
