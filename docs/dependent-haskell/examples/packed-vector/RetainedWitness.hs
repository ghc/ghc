{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module RetainedWitness (witness) where

import PackedVecLibrary (Nat(..))
import RetainedNat (Retained(..))

-- | Return the stored index without inspecting its payload.
witness :: Retained p -> Nat
witness (Retain n _) = n
