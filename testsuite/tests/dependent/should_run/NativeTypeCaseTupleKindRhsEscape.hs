{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseTupleKindRhsEscape where

import GHC.TypeNats (Nat)

type BadKind (dims :: ((Bool, Nat), Nat)) =
  let ((flag, _), _) = (dims :: ((Bool, ghost), Nat))
  in case flag of
    'True -> Int
    'False -> Bool
