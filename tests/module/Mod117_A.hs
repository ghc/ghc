-- re-exporting m2 outside of T(..)
module Mod117_A( T(T,m1), m2) where

data T = T { m1 :: Int, m2 :: Int}

