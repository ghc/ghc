{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeAbstractions          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Numeric.Dimensions.Dim  where

import Data.Kind
import Data.Type.Equality      ((:~:)(..))
import GHC.TypeLits  (Nat, TypeError, ErrorMessage (..))
import Unsafe.Coerce (unsafeCoerce)


-- | Type-level dimensionality
type Dim :: forall k. k -> Type
data Dim (ns :: k) where
  D    :: Dim '[]
  (:*) :: forall {k} (n::Nat) (ns::[k]) . Dim n -> Dim ns -> Dim (ConsDim n ns)
  Dn   :: forall (n :: Nat) . Dim (n :: Nat)

infixr 5 :*

-- | Either known or unknown at compile-time natural number
data XNat = XN Nat | N Nat
-- | Unknown natural number, known to be not smaller than the given Nat
type XN (n::Nat) = 'XN n
-- | Known natural number
type N (n::Nat) = 'N n

-- | List concatenation
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    (++) '[] bs = bs
    (++) as '[] = as
    (++) (a ': as) bs = a ': (as ++ bs)
infixr 5 ++

type family Head (xs :: [k]) :: k where
    Head (x ': xs) = x
    Head '[]       = TypeError ( 'Text
      "Head -- empty type-level list."
     )

type family Tail (xs :: [k]) :: [k] where
    Tail (x ': xs) = xs
    Tail '[]       = TypeError ( 'Text
      "Tail -- empty type-level list."
     )

-- | Unify usage of XNat and Nat.
--   This is useful in function and type definitions.
--   Mainly used in the definition of Dim.
type ConsDim :: forall l k. l -> [k] -> [k]
type family ConsDim (x :: l) (xs :: [k]) = (ys :: [k]) | ys -> x xs l where
    ConsDim (x :: Nat) (xs :: [Nat])  = x    ': xs
    ConsDim (x :: Nat) (xs :: [XNat]) = N x  ': xs
    ConsDim (XN m)     (xs :: [XNat]) = XN m ': xs

concatDim :: forall (xs :: [Nat]) (ys :: [Nat]) . Dim xs -> Dim ys -> Dim (xs ++ ys)
concatDim D ys         = ys
concatDim xs D         = xs
concatDim (x :* xs) ys = case unsafeCoerce Refl :: (xs ++ ys) :~: (Head xs ': (Tail xs ++ ys)) of
      Refl -> x :* concatDim xs ys
