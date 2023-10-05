{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}

module T8917 where

data Nat = Zero | Succ Nat
type family a + b where
  Zero + a = a
  (Succ n) + m = Succ (n + m)
