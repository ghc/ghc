{-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}

-- This wrongly fails with
-- 
--   Can't construct the infinite type n = PLUS n ZERO

module GADT1 where

import Data.Kind

data ZERO
data SUCC (n :: Type)

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

{-

From Succ branch of plus_zero

[G] n ~ SUCC n1     -- n1 existentially bound
[G] PLUS n1 ZERO ~ n1

[W] PLUS n ZERO ~ n

--> [W] PLUS (SUCC n1) ZERO ~ SUCC n1
--> [W] SUCC (PLUS n1 ZERO) ~ SUCC n1
--> [W] SUCC n1 ~ SUCC n1

-}