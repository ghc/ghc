

-- Test for top-level duplicates

module Dup where

data T = MkT | MkT

data S = MkT

data P = MkP { rf :: Int, rf :: Int }
data Q = MkQ { rf :: Int }

class C a where
  data CT a 
  f :: CT a -> a
  data CT a
  f :: CT a -> a

