module Mod159_A (C(..)) where

class C a where
  m1 :: a -> Int
  m2 :: a -> Int
  m3 :: a -> Int

instance C Char where
  m1 _ = 1
  m2 _ = 2
  m3 _ = 3


