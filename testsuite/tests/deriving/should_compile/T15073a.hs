{-# LANGUAGE UnboxedTuples #-}
module T15073a where

class P a where
  p :: a -> (# a #)
