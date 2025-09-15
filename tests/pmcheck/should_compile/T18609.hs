{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns, GADTs, DataKinds, KindSignatures, EmptyCase #-}

-- | All examples from https://arxiv.org/abs/1702.02281
module GarrigueLeNormand where

import Data.Kind

data N = Z | S N

data Plus :: N -> N -> N -> Type where
  PlusO :: Plus Z a a
  PlusS :: !(Plus a b c) -> Plus (S a) b (S c)

data SMaybe a = SJust !a | SNothing

trivial :: SMaybe (Plus (S Z) Z Z) -> ()
trivial SNothing = ()

trivial2 :: Plus (S Z) Z Z -> ()
trivial2 x = case x of {}

easy :: SMaybe (Plus Z (S Z) Z) -> ()
easy SNothing = ()

easy2 :: Plus Z (S Z) Z -> ()
easy2 x = case x of {}

harder :: SMaybe (Plus (S Z) (S Z) (S Z)) -> ()
harder SNothing = ()

harder2 :: Plus (S Z) (S Z) (S Z) -> ()
harder2 x = case x of {}

invZero :: Plus a b c -> Plus c d Z -> ()
invZero !_     !_     | False = ()
invZero  PlusO  PlusO = ()

data T a where
  A :: T Int
  B :: T Bool
  C :: T Char
  D :: T Float

data U a b c d where
  U :: U Int Int Int Int

f :: T a -> T b -> T c -> T d
  -> U a b c d
  -> ()
f !_ !_ !_ !_ !_ | False = ()
f  A  A  A  A  U = ()

g :: T a -> T b -> T c -> T d
  -> T e -> T f -> T g -> T h
  -> U a b c d
  -> U e f g h
  -> ()
g !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ | False = ()
g  A  A  A  A  A  A  A  A  U  U = ()
