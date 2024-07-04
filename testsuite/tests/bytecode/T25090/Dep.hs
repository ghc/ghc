module Dep where

data A = A Int

used :: Int
used = 9681

dep :: A
dep = A used

unused1 :: A
unused1 = A 1

unused2 :: A
unused2 = unused1
