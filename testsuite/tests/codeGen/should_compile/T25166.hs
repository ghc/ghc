module Test where

data A = A | B | C

data D = D !Bool {-# UNPACK #-} !A

foo = D True B
