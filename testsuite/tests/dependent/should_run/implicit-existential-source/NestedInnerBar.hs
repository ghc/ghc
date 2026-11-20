{-# LANGUAGE ExperimentalDependentTypes #-}
module NestedInnerBar where

import Vector

type Mixed a = exists (n :: Nat). exists (m :: Nat) | (Vec n a, Vec m a)

bad :: Mixed a -> Bool
bad p = observe p
  where
    observe :: (Vec n a, Vec m a) -> Bool
    observe _ = True
