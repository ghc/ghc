{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T18191 where

data Z a where
  MkZ1 :: forall a. forall b. { unZ1 :: (a, b) } -> Z (a, b)
  MkZ2 :: Eq a => Eq b => { unZ1 :: (a, b) } -> Z (a, b)
