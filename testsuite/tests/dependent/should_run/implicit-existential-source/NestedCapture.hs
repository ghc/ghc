{-# LANGUAGE ExperimentalDependentTypes, RankNTypes #-}
module NestedCapture where

import Vector

type Captured = forall a. (exists (n :: Nat). exists (m :: Nat). (Vec n a, Vec m a), Vec m a)
