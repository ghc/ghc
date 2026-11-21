{-# LANGUAGE GADTs #-}
module GadtTrailingContextScope where

data T a b where
  Bad :: forall b. a -> forall a. Eq a => T a b
