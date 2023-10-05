{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
module ShouldCompile where

data T where
  C1 :: Int %1 -> T
  C2 :: Int %m -> T
  C3 :: Int -> T
