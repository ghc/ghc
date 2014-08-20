{-# LANGUAGE FlexibleInstances #-}
module SafeInfered05_A where

class C a where
  f :: a -> String

instance C [Int] where
  f _ = "[Int]"

