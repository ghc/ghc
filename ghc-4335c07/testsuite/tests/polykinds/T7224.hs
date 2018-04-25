{-# LANGUAGE PolyKinds #-}

module T7224 where

class PMonad' (m :: i -> i -> * -> *) where
  ret'  :: a -> m i i a
  bind' :: m i j a -> (a -> m j k b) -> m i k b
