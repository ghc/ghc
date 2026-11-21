{-# LANGUAGE GADTs, ScopedTypeVariables, TypeAbstractions #-}
module GadtOrderedTelescopeScope where

data Mixed a where
  Mixed :: Int -> forall a. Eq a => a -> Mixed a

wrong :: Mixed a -> Bool
wrong (Mixed n @a x @b) = x == x

data Fenced a where
  Fenced :: Ord a => (a, a) -> (a ~ Int) => Fenced a

badField :: Fenced a -> Int
badField (Fenced (x :: (Int, Int))) = fst x
