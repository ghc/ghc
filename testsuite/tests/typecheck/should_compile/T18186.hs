{-# LANGUAGE TypeFamilies, DataKinds, KindSignatures, TypeOperators #-}

module T18186 where

import GHC.TypeLits (Nat, Natural, Symbol)
import Data.Type.Equality ((:~:)(..))
import Data.Proxy

nat_is_natural :: Nat :~: Natural
nat_is_natural = Refl

data NatPair = TN Natural Natural

type X = TN 1 101

type family SecondNat (a :: NatPair) :: Nat where
    SecondNat ('TN _ a) = a

