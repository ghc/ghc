
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnliftedDatatypes #-}

module EtaExpandDataCon where

import Data.Coerce
import Data.Kind
import GHC.Exts

-- Simple eta-expansion tests.

type D1 :: Type -> Type
data D1 a where
  MkD1 :: Ord a => Float# -> Int -> a %1 -> D1 a

foo1 :: Ord a => Float# -> Int -> a -> D1 a
foo1 x1 = MkD1 ( x1 `powerFloat#` 1234.0# )
  -- Only the last argument needs us to change the multiplicity,
  -- but this means adding lambdas for intervening arguments:
  -- foo x1 = \ x2 x3 -> MkG x1 x2 x3

type D2 :: Type -> Type -> Type
data D2 a b where
  MkD2 :: forall a b. a %1 -> b %1 -> a %1 -> D2 a b

foo2 :: forall c d. (c -> c) -> c -> d -> c -> D2 c d
foo2 very_big arg0 = MkD2 (very_big arg0)

type N3 :: TYPE r -> Type
newtype N3 a where
  MkN3 :: forall r (a :: TYPE r). (a %1 -> N3 a) %1 -> N3 a

foo3 :: (a %1 -> N3 a) -> N3 a
foo3 = MkN3

type D4 :: TYPE FloatRep -> Type -> Type
data D4 a b = MkD4 a b b

foo4 :: Bool -> Bool -> D4 Float# Bool
foo4 = MkD4 ( 9.0# `timesFloat#` 17.0# )

-- Nightmare stress test with all features:
--
--  - Boxed dictionary and equality constraints
--  - GADT equality constraints
--  - unpacking
--  - levity-polymorphic result kind

data Unpackable = Unpackable Double# Double# Double# Double#

type F :: k -> k
type family F a = r | r -> a where

type G :: Type -> forall k. k -> Type -> Type -> forall l -> TYPE (BoxedRep l)
data G a b c d l where
  MkG :: (Ord a, F Int ~ Bool, Coercible (F Bool) Char, Eq x)
      => Float#
      -> {-# UNPACK #-} !Unpackable
      -> {-# UNPACK #-} !Unpackable
   %1 -> a
   %1 -> (a -> x)
   %1 -> x
   %1 -> G a (F b) a Double l

bar :: (F Bool ~ Char, F Int ~ Bool, Ord a)
    => Unpackable
 %1 -> a
    -> (a -> Int)
 %1 -> Int
    -> G a (F b) a Double Unlifted
bar = MkG 1728.0# (Unpackable 1.0## 2.0## 3.0## 4.0##)
