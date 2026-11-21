{-# LANGUAGE GADTs #-}

module CurriedGadtContextsProvider (Packed(..)) where

data Packed where
  Curried :: forall a. Eq a => (Show a => a -> a -> Packed)
  Plain :: Eq a => Show a => a -> a -> Packed
