{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -O2 -fspecialise-aggressively #-}

-- This pragma is just here to pretend that the function body of 'foo' is huge
-- and should never be inlined.
{-# OPTIONS_GHC -funfolding-use-threshold=-200 #-}

module T25703a where

import Data.Kind
import Data.Type.Equality
import Data.Proxy
import GHC.TypeNats

-- Pretend this is some big dictionary that absolutely must get
-- specialised away for performance reasons.
type C :: Nat -> Constraint
class C i where
  meth :: Proxy i -> Double
instance C 0 where
  meth _ = 0.1
instance C 1 where
  meth _ = 1.1
instance C 2 where
  meth _ = 2.1

{-# INLINEABLE foo #-}
foo :: forall a (n :: Nat) (m :: Nat)
    .  ( Eq a, C n, C m )
    => a -> ( Proxy n, Proxy m ) -> Int -> Double
-- Pretend this is a big complicated function, too big to inline,
-- for which we absolutely must specialise away the 'C n', 'C m'
-- dictionaries for performance reasons.
foo a b c
  = if a == a
    then meth @n Proxy + fromIntegral c
    else 2 * meth @m Proxy

-- Runtime dispatch to a specialisation of 'foo'
foo_spec :: forall a (n :: Nat) (m :: Nat)
         .  ( Eq a, KnownNat n, KnownNat m )
         => a -> ( Proxy n, Proxy m ) -> Int -> Double
foo_spec a b c
  | Just Refl <- sameNat @n @0 Proxy Proxy
  , Just Refl <- sameNat @m @0 Proxy Proxy
  = foo @a @0 @0 a b c
  | Just Refl <- sameNat @n @0 Proxy Proxy
  , Just Refl <- sameNat @m @1 Proxy Proxy
  = foo @a @0 @1 a b c
  | Just Refl <- sameNat @n @1 Proxy Proxy
  , Just Refl <- sameNat @m @1 Proxy Proxy
  = foo @a @1 @1 a b c
  | Just Refl <- sameNat @n @0 Proxy Proxy
  , Just Refl <- sameNat @m @2 Proxy Proxy
  = foo @a @0 @2 a b c
  | Just Refl <- sameNat @n @1 Proxy Proxy
  , Just Refl <- sameNat @m @2 Proxy Proxy
  = foo @a @1 @2 a b c
  | Just Refl <- sameNat @n @2 Proxy Proxy
  , Just Refl <- sameNat @m @2 Proxy Proxy
  = foo @a @2 @2 a b c
  | otherwise
  = error $ unlines
      [ "f: no specialisation"
      , "n: " ++ show (natVal @n Proxy)
      , "m: " ++ show (natVal @m Proxy)
      ]
