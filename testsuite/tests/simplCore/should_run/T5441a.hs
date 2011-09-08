module T5441a where

import Unsafe.Coerce (unsafeCoerce)
import GHC.Prim (Any)

listmap :: (a -> b) -> [a] -> [b]
listmap f []       = []
listmap f (x : xs) = f x : listmap f xs

data Nat = Z | S Nat

{-# NOINLINE inject #-}
inject :: Nat -> Nat -> Nat
inject m i = i

{-# NOINLINE look #-}
look :: Nat -> String -> Char
look Z _ = '0'

showDigit :: Nat -> () -> Nat -> Char
showDigit base prf d = look (inject base d) ""

toDigits :: Nat -> Nat -> [Nat]
toDigits Z Z = [Z]

coe1 :: (Nat -> String) -> Any
coe1 = unsafeCoerce

coe2 :: Any -> (Nat -> String)
coe2 = unsafeCoerce

showInBase :: Nat -> Any
showInBase base
  = coe1 (\n -> listmap
                (showDigit base ())
                (toDigits base n))

showNat :: Nat -> String
showNat = coe2 (showInBase Z)
