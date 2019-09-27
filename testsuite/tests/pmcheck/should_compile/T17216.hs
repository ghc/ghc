module Lib where

foo :: Int -> Bool
foo i
  | i < 0 = True
  | i == 0 = True
  | i > 0 = True

