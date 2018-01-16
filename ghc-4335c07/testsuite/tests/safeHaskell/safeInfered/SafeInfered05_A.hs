{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-safe #-}
module SafeInfered05_A where

class C a where
  f :: a -> String

instance C [Int] where
  f _ = "[Int]"

