{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module T12087a where

data F1 a where
  MkF1 :: Ord a => Eq a => a -> F1 a
