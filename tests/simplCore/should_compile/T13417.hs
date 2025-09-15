module T13417 where

-- Amazingly this crashed GHC 8.0.2

data T a = E7

cons7 :: T a -> T b
cons7 E7 = E7
