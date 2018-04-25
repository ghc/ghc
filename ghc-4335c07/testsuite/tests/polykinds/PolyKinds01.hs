{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}

module PolyKinds01 where

data Nat = Ze | Su Nat

data Vec :: * -> Nat -> * where
  VNil  :: Vec a Ze
  VCons :: a -> Vec a n -> Vec a (Su n)

vec :: Vec Nat (Su Ze)
vec = VCons Ze VNil
