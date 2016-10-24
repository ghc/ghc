{-# LANGUAGE TypeInType #-}

module T12081 where

data Nat = Z | S Nat

class C (n :: Nat) where
  type T n :: Nat
  f :: (a :: T n)
