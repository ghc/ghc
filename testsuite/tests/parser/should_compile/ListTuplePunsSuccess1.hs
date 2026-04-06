{-# language TypeFamilies, DataKinds, UnboxedTuples, MagicHash #-}

module ListTuplePunsSuccess1 where

import Data.Kind (Type)
import Data.List (List)
import Data.Tuple.Experimental (Unit, Tuple2, Tuple, Constraints, Tuple#)
import GHC.Exts (TYPE, RuntimeRep(IntRep))

type family F (a :: Tuple2 Type Type) :: Type where
  F (a, _) = a

type X = F (Int, Double)

x :: X
x = (5 :: Int)

type family G (a :: Unit) :: Type where
  G () = Unit

type Y = G ()

type family H (a :: List Type) :: Type where
  H [] = Char
  H (a : _) = a

type Z0 = H [Int]

z0 :: Z0
z0 = (5 :: Int)

type Z1 = H [Int, Double]

z1 :: Z1
z1 = (5 :: Int)

swap :: Tuple (a, b) -> Tuple (b, a)
swap (x, y) = (y, x)

swap_mono :: Tuple (Int, Bool) -> Tuple (Bool, Int)
swap_mono (x, y) = (y, x)

swap_inc :: Constraints (Num a, Num b) => Tuple (a, b) -> Tuple (b, a)
swap_inc (x, y) = (y + 1, x + 1)

swap# :: forall (a :: TYPE IntRep) (b :: Type). Tuple# (a, b) -> Tuple# (b, a)
swap# (# x, y #) = (# y, x #)
