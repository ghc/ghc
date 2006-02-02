{-# OPTIONS -fglasgow-exts #-}

-- Triggered a desugaring bug in earlier verison

module Shouldcompile where

data T a where
  T1 :: Int -> T Int

f :: (T a, a) -> Int
f (T1 x, z) = z
