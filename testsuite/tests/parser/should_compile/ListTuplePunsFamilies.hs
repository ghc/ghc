{-# language UnboxedTuples, UnboxedSums, MagicHash, DataKinds #-}

module ListTuplePunsFamilies where

import Data.Tuple.Experimental
import Data.Sum.Experimental

b1 :: Int
b1 =
  case (5, 6) :: Tuple (Int, Double) of
    (_, _) -> 5

ut :: Int
ut =
  case (# 5, 6 #) :: Tuple# (Int, Double) of
    (# _, _ #) -> 5

us :: Int
us =
  case (# 5 | #) :: Sum# (Int, Double) of
    (# a | #) -> a

c :: Constraints (Monad m, Eq a) => a -> m Bool
c a = pure (a == a)

c1 :: Monad m => Eq a => a -> m Bool
c1 = c

c2 :: IO Bool
c2 = c1 (5 :: Int)
