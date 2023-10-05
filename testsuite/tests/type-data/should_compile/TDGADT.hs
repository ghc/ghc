{-# LANGUAGE TypeData #-}
module TDGADT where

import Data.Kind (Type)

type data Nat = Zero | Succ Nat

-- type level GADT
type data Vec :: Nat -> Type -> Type where
    VNil :: Vec Zero a
    VCons :: a -> Vec n a -> Vec (Succ n) a

type X = VCons Bool (VCons Int VNil)
