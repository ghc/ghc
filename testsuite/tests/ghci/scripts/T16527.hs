{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T16527 where

data T where
  MkT1 :: (Int -> Int) -> T
  MkT2 :: (forall a. Maybe a) -> T
