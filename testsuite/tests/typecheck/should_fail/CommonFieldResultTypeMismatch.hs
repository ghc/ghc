module CommonFieldResultTypeMismatch where

data A a where
  A1 :: { fld :: Int } -> A Int
  A2 :: { fld :: Int } -> A Double
