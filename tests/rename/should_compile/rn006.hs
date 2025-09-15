-- !!! rn006: two sets of mutually-recursive blobs:
-- !!!  f, g, h are mut rec
-- !!!  i, j, k are mut rec

module Test where

f x = g x x
i x = j x x

g x y = h x x y
j x y = k x x y

h x y z = f z
k x y z = i z
