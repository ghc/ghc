{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}

module PolyKinds11 where

-- Test inference

data Nat = Ze | Su Nat

data Vec a n where -- Vec :: * -> Nat -> *
  VNil  :: Vec a Ze
  VCons :: a -> Vec a n -> Vec a (Su n)
