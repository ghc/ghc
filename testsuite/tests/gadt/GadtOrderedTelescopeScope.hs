{-# LANGUAGE GADTs, TypeAbstractions #-}
module GadtOrderedTelescopeScope where

data Mixed a where
  Mixed :: Int -> forall a. Eq a => a -> Mixed a

wrong :: Mixed a -> Bool
wrong (Mixed n @a x @b) = x == x
