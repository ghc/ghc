{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T12087 where

data F1 a where
  MkF1 :: Ord a => Eq a => a -> F1 a

data F2 a where
  MkF2 :: Ord a => a -> Eq a => F2 a

data F3 a where
  MkF3 :: forall a. Eq a => a -> forall b. Eq b => b -> F3 a

data F4 a where
  MkF4 :: forall a b. Eq a => a -> Eq b => b -> F4 a

data F5 a where
  MkF5 :: Int -> Int -> forall a. a -> Int -> Int -> forall b. b -> F5 a
