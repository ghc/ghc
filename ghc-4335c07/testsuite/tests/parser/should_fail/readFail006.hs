-- !!! Using pattern syntax on RHS
module ShouldFail where

f :: Int -> Int
f x = _

g :: Int -> Int
g x = 2 + 2@_

