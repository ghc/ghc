{-# OPTIONS_GHC -Wall #-}
module Completesig07 where

-- Some overlapping examples

data T = A | B | C
data S = D | E | F

{-# COMPLETE A, B #-}
{-# COMPLETE D #-}

m1 :: T -> ()
m1 A = ()
m1 A = ()
m1 B = ()

m2 :: T -> S -> ()
m2 A D = ()
m2 B D = ()
m2 A D = ()

m3 :: T -> ()
m3 C = ()
m3 C = ()
