{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}
module RetainedSumOperator (sumIndex, successorIndex) where

import PackedVecLibrary (Nat, Vec, replicateNative)
import Prelude (Bool(True), (+))
import qualified Prelude as P

sumIndex :: foreach (x :: Nat) -> foreach (y :: Nat) -> Vec Bool (x P.+ y)
sumIndex x y = replicateNative (x + y) True

successorIndex :: foreach (y :: Nat) -> Vec Bool (1 P.+ y)
successorIndex y = replicateNative (1 + y) True
