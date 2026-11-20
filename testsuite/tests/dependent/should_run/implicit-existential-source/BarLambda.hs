{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module BarLambda where

import Vector

type Builder = exists (n :: Nat) | Bool -> Vec n Bool

bad :: Builder
bad = \b -> Cons b Nil
