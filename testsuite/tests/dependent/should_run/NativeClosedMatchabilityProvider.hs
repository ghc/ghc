{-# LANGUAGE DataKinds, TypeFamilies #-}
module NativeClosedMatchabilityProvider (K, Forward) where

import Data.Kind (Type)

type family K (b :: Bool) :: Type where
  K 'True = Type -> Type
  K 'False = Bool -> Bool

type Forward b = K b
