{-# OPTIONS_GHC -Wall #-}
module Completesig08 where

-- Nested matching

data T = A S | B
data S = D | E

{-# COMPLETE A #-}
{-# COMPLETE D #-}

m1 :: T -> ()
m1 (A D) = ()
