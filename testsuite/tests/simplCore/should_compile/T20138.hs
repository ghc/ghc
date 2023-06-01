module T20138 where

f :: Int -> Int
f n = case n of
  2 -> n
  n -> n

g :: Int -> Int
g n = case n of
  2 -> 2
  n -> n

h :: Int -> Int
h n = case n of
  2 -> maxBound
  n -> n

data O = O !Ordering

k :: O -> O
k (O LT) = O LT
k (O o)  = O o
