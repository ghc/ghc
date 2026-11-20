{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
module NativeDepFunCoClient (observe) where

import NativeDepFunCoProvider

{-# NOINLINE produce #-}
produce :: foreach (f :: SourceToken -> Nat)
        -> foreach (n :: SourceToken) -> Vec (f n)
produce _ _ = Vec 9

observe :: TargetToken -> Int
observe n =
    let indexAsNat :: SourceToken -> Nat
        indexAsNat _ = Z
    in case transport indexAsNat (produce indexAsNat) n of
        Vec value -> value
