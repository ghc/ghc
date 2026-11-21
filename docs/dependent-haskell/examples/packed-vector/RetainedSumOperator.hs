{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}
module RetainedSumOperator (appendByMethod, sumIndex, successorIndex) where

import PackedVecLibrary (Nat, Vec, appendNative, replicateNative)
import Prelude (Bool(True), (+))
import qualified Prelude as P

sumIndex :: foreach (x :: Nat) -> foreach (y :: Nat) -> Vec Bool (x P.+ y)
sumIndex x y = replicateNative (x + y) True

successorIndex :: foreach (y :: Nat) -> Vec Bool (1 P.+ y)
successorIndex y = replicateNative (1 + y) True

-- | Append with the result length written using the selected Num method.
appendByMethod
    :: foreach (n :: Nat) -> foreach (m :: Nat)
    -> Vec a n -> Vec a m -> Vec a (n P.+ m)
appendByMethod n m xs ys = appendNative n m xs ys
