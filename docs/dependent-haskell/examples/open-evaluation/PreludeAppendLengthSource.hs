{-# LANGUAGE DataKinds, ExperimentalDependentTypes, StandaloneKindSignatures #-}
module PreludeAppendLengthSource (Appended, Plus, plus) where

import qualified Prelude as P
import PreludeMapLengthSize (Nat, plus)

-- | Apply the installed append function to two lists.
type Appended :: forall a. [a] -> [a] -> [a]
type Appended xs ys = (P.++) xs ys

-- | Use the checked equations of natural number addition.
type Plus :: Nat -> Nat -> Nat
type Plus m n = plus m n
