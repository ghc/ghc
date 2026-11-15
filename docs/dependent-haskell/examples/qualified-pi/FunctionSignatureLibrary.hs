{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Export one length-preserving worker with actual retained function binders.
module FunctionSignatureLibrary (idCall) where

import PackedVecLibrary

-- | Return the original vector without evaluating its function or input index.
idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs
