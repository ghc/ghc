{-# OPTIONS_GHC -Wall #-}
module Completesig10 where

-- Multiple competing COMPLETE sigs AHHH!!

data T = A | B | C | D | E

{-# COMPLETE A,B #-}
{-# COMPLETE C,D #-}

-- Completely overlapping
m1 :: T -> ()
m1 A = ()
m1 B = ()
m1 C = ()
m1 D = ()

-- Incomplete overlap
m2 :: T -> ()
m2 B = ()
m2 D = ()

-- Redundant incomplete overlap
m3 :: T -> ()
m3 B = ()
m3 C = ()
m3 D = ()

-- One matches

m4 :: T -> ()
m4 C = ()
m4 D = ()
