{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, TypeFamilies #-}

module NativeNestedSumFamilyReturn where

import Data.Kind (Type)

type Loop a = some (left :: Bool) |
  (some (right :: Bool) | Return a left)

type family Return (a :: Type) (left :: Bool) :: Type where
  Return a left = Loop a
