module T12686b where

data Bad = MkBad {
  a :: Int,
  b :: Either Int 'a }
-- The 'a should be rejected in a civilised way