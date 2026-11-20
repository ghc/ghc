{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications #-}

module HiddenName where

import Vector

atLength :: forall n. Vec n Bool -> Vec n Bool
atLength xs = xs

bad :: exists (n :: Nat). Bool -> Vec n Bool
bad = \b -> atLength @n (Cons b Nil)
