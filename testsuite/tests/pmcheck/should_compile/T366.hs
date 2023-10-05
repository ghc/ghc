{-# OPTIONS_GHC -XGADTs -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T366 where

data T a where
  C1 :: T Char
  C2 :: T Float

exhaustive :: T Char -> Char
exhaustive C1 = ' '
