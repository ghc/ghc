{-# OPTIONS_GHC -O #-}
  -- #11248, comment:6 showed that this tests failed with -O

{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies,
             KindSignatures, ConstraintKinds #-}

module T11248 where

import GHC.TypeLits

type a / b = FDiv a b
type a ** b = FMul a b

type family FDiv a b where
  FDiv 11648 128 = 91

type family FMul a b where
  FMul 64 91 = 5824

type family FGCD a b where
  FGCD 128 448 = 64
  FGCD 128 5824 = 64

type family FLCM a b where
  FLCM 128 5824 = 11648

data CT (m :: Nat) (m' :: Nat)
type H0 = 128
type H1 = 448
type H0' = 11648
type H1' = 5824

main' = let x = undefined :: CT H0 H0'
        in foo x :: CT H1 H1'

foo x = bug x

type Ctx2 e r s e' r' =
  (e ~ FGCD r e', r' ~ FLCM r e', e ~ FGCD r s)

type Ctx1 e r s e' r' =
  (Ctx2 e r s e' r', e' ~ (e ** (r' / r)))

bug :: (Ctx1 e r s e' r')
  => CT r r' -> CT s s'
bug = undefined
