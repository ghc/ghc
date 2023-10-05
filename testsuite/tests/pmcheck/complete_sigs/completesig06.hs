{-# OPTIONS_GHC -Wall #-}
module Completesig06 where

-- Some non-exhaustive examples

data T = A | B | C
data S = D | E | F

{-# COMPLETE A, B #-}
{-# COMPLETE D #-}

m1 :: T -> ()
m1 A = ()

m2 :: T -> ()
m2 B = ()
m2 C = ()

m3 :: T -> ()
m3 C = ()

m4 :: T -> S -> ()
m4 A E = ()
m4 A F = ()
m4 B F = ()
m4 B E = ()

m5 :: T -> S -> ()
m5 C D = ()
