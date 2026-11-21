{-# LANGUAGE NoExperimentalDependentTypes, PolyKinds, TypeFamilies #-}

module NativeAssociatedSumClass (Build (..), Shape (..)) where

import Data.Kind (Type)

class Build (a :: Type) where
    type Result a (b :: Type)

class Shape (a :: Type) where
    type Keep a (k :: Type) (p :: k)
