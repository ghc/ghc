-- re-exporting m2 outside of C(..)
module Mod118_A( C(m1), m2) where

class C a where
  m1 :: a -> Int
  m2 :: a -> Bool

instance C Int where
  m1 _ = 1
  m2 _ = True

