{-# LANGUAGE PolyKinds, TypeAbstractions #-}

module T7224 where

import Data.Kind

type PMonad' :: (i -> i -> Type -> Type) -> Constraint
class PMonad' @i m where
  ret'  :: a -> m i i a
  bind' :: m i j a -> (a -> m j k b) -> m i k b
