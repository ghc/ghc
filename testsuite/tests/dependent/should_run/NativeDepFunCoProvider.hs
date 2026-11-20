{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies #-}
module NativeDepFunCoProvider (Nat(..), Vec(..), SourceToken, TargetToken, transport) where

import Data.Kind (Type)

data Nat = Z
data Vec (n :: Nat) = Vec Int
data Token (a :: Type) = Token
type family Normalize (a :: Type) :: Type where
    Normalize Bool = Bool
type SourceToken = Token (Normalize Bool)
type TargetToken = Token Bool

transport :: foreach (f :: SourceToken -> Nat)
          -> (foreach (n :: SourceToken) -> Vec (f n))
          -> foreach (n :: TargetToken) -> Vec (f n)
transport f value = value
