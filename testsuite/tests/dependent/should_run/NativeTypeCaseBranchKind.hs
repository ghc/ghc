{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseBranchKind where

type F a = case a of
  True -> Int
  False -> 'False
