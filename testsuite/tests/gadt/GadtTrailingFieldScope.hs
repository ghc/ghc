{-# LANGUAGE GADTs #-}
module GadtTrailingFieldScope where

data N where
  N :: a -> (Eq a, Num a) => N

wrong :: N -> Bool
wrong (N 0) = True
wrong (N _) = False
