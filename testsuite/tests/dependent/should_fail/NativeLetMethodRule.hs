{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module NativeLetMethodRule where

import PackedVecLibrary

data Input where
    Input :: Eq a => a -> Input

-- The case alternative uses the equality for the enclosing local product.
build :: Input -> foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool (times rows cols)
build box rows cols =
    let cells = rows * cols
    in case box of Input x -> replicateNative cells (x == x)

{-# RULES "change local multiplication" forall (n :: Nat) (m :: Nat). n * m = Z #-}
