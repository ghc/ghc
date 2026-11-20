{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}

module NativeNestedSumRecursiveAlias where

type Loop a = some (left :: Bool) |
  (some (right :: Bool) | Loop a)
