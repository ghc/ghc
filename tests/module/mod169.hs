-- !!! Non-clashing local binding and occurrence.
module M where

m :: Int
m = let length _ = 5 in length []
