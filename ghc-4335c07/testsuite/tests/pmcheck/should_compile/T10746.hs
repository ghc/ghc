{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs, DataKinds #-}

module Test where

-- Non-exhaustive (missing True & False)
test :: Bool -> Int
test a = case a of

data Void

-- Exhaustive
absurd :: Void -> a
absurd a = case a of {}

data Nat = Zero | Succ Nat

data Fin n where
  FZ ::          Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- Exhaustive
f :: Fin Zero -> a
f x = case x of {}
