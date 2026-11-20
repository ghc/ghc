{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications #-}

module Shadowed where

import Vector

atLength :: forall n. Vec n Bool -> Vec n Bool
atLength xs = xs

bad :: forall (n :: Nat). exists (n :: Nat). Bool -> Vec n Bool
bad = \b -> atLength @n (Cons b Nil)
