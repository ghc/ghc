{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T18191 where

data T where
  MkT :: (forall a. a -> b -> T)

data S a where
  MkS :: (forall a. S a)

data U a where
  MkU :: (Show a => U a)
