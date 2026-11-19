{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, TypeApplications #-}
module NativeRetainedCastLength where

import Data.Coerce (coerce)
import PackedVecLibrary (Nat(..), Vec)

bad :: (foreach (n :: Nat) -> Vec Int n) -> (foreach (n :: Nat) -> Vec Int ('S n))
bad f n = coerce @(Vec Int n) @(Vec Int ('S n)) (f n)
