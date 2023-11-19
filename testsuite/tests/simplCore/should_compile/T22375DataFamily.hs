{-# LANGUAGE GHC2021, TypeFamilies, StandaloneKindSignatures #-}

module T22375DataFamily where

import Data.Kind

type X :: Type -> Type
data family X a
data instance X ()
  = A | B | C | D | E
  | F | G | H | I | J
  deriving Eq

f :: X () -> Int -> Int
f x v
  | x == A = v + 1
  | x == B = v + 2
  | x == C = v + 3
  | x == D = v + 4
  | x == E = v + 5
  | x == F = v + 6
  | x == G = v + 7
  | x == H = v + 8
  | x == I = v + 9
  | otherwise = v + 10
