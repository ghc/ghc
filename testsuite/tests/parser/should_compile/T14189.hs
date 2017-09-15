module T14189
  (
  MyType (f,NT)
  ) where

data MyType = MT Int | NT | F { f :: Int }
