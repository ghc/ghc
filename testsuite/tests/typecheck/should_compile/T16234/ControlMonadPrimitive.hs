{-# LANGUAGE TypeFamilies #-}
module ControlMonadPrimitive (PrimMonad(..)) where

import Control.Monad.Trans.State (StateT)

class Monad m => PrimMonad m where
  type PrimState m
instance PrimMonad m => PrimMonad (StateT s m) where
  type PrimState (StateT s m) = PrimState m
