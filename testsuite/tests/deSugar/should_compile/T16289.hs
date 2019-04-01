module Lib where

data Value = Finite Integer | Infinity
  deriving (Eq)

instance Num Value where
  (+)         = undefined
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  negate      = undefined
  fromInteger = Finite

isSpecial :: Value -> Bool
isSpecial Infinity = True
isSpecial 0        = True
-- `isSpecial _` should not elicit an overlapping patterns warning
isSpecial 1        = False
