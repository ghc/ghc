{-# OPTIONS_GHC -Wall #-}
module Completesig08 where

-- Some redundant examples

data T = A | B | C
data S = D | E | F

{-# COMPLETE A, B #-}
{-# COMPLETE D #-}

m1 :: T -> ()
m1 A = ()
m1 B = ()
m1 C = ()

m2 :: T -> S -> ()
m2 A D = ()
m2 B D = ()
m2 C D = ()

m3 :: T -> S -> ()
m3 A D = ()
m3 B D = ()
m3 A E = ()
m3 A F = ()

m4 :: S -> ()
m4 D = ()
m4 E = ()
