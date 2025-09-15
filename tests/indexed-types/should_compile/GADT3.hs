{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}

-- Panics in bind_args

module GADT3 where

data EQUAL x y where
  EQUAL :: x~y => EQUAL x y

data ZERO
data SUCC n

data Nat n where
  Zero :: Nat ZERO
  Succ :: Nat n -> Nat (SUCC n)

type family PLUS m n
type instance PLUS ZERO n = n

plus_zero :: Nat n -> EQUAL (PLUS ZERO n) n
plus_zero Zero = EQUAL
plus_zero (Succ n) = EQUAL

data FOO n where
  FOO_Zero :: FOO ZERO

foo :: Nat m -> Nat n -> FOO n -> FOO (PLUS m n)
foo Zero n s = case plus_zero n of EQUAL -> s

