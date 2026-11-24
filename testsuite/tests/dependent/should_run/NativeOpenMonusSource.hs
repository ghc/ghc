module NativeOpenMonusSource (monus) where

import PreludeMapLengthSize (Nat (..))

monus :: Nat -> Nat -> Nat
monus m        Zero     = m
monus Zero     (Succ _) = Zero
monus (Succ m) (Succ n) = monus m n
