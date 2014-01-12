{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}

-- This wrongly fails with
-- 
--   Can't construct the infinite type n = PLUS n ZERO

module GADT1 where

data ZERO
data SUCC n

data Nat n where
  Zero :: Nat ZERO
  Succ :: Nat n -> Nat (SUCC n)

type family PLUS m n
type instance PLUS ZERO n = n
type instance PLUS (SUCC m) n = SUCC (PLUS m n)

data EQUIV x y where
  EQUIV :: EQUIV x x

plus_zero :: Nat n -> EQUIV (PLUS n ZERO) n
plus_zero Zero = EQUIV
plus_zero (Succ n) = case plus_zero n of
                       EQUIV -> EQUIV 

