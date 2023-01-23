module RnDefaultSigFail where

class C a where
  m :: a
  default m :: Num a => a
  m = 0
