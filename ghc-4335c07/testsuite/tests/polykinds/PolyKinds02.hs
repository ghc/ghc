{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}

module PolyKinds02 where

data Nat = Ze | Su Nat

data Vec :: * -> Nat -> * where
  VNil  :: Vec a Ze
  VCons :: a -> Vec a n -> Vec a (Su n)

vec :: Vec Nat Nat -- Kind error
vec = vec
