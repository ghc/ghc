{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, TypeOperators #-}
module NativeMethodGiven where

import PackedVecLibrary

withGiven :: Num Nat => foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
withGiven n m = replicateNative @Bool (n + m) True

withLiteral :: Num Nat => Vec Bool 3
withLiteral = replicateNative @Bool 3 True
