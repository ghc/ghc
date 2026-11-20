{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module BarProvider (Bar) where

import Vector

type Bar a = exists (n :: Nat) | Vec n a
