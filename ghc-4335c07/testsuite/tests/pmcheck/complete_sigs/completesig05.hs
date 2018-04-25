{-# OPTIONS_GHC -Wall #-}
module Completesig05 where

-- Matching against multiple arguments

data T = A | B | C
data S = D | E | F

{-# COMPLETE A, B #-}
{-# COMPLETE D #-}

match :: T -> S -> ()
match A D = ()
match B D = ()
