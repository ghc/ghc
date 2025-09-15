-- Test desugaring of mutual recursion of many functions
-- which generated a big-tuple link error in GHC 6.0

module Main where


a1 :: (Num a, Eq a) => a -> a

a1 x | x==0 = x
a1 x = 1 + k8 (x-1)

a2 x = 1 + a1 x
a3 x = 1 + a2 x
a4 x = 1 + a3 x
a5 x = 1 + a4 x
a6 x = 1 + a5 x
a7 x = 1 + a6 x
a8 x = 1 + a7 x

b1 x = 1 + a8 x
b2 x = 1 + b1 x
b3 x = 1 + b2 x
b4 x = 1 + b3 x
b5 x = 1 + b4 x
b6 x = 1 + b5 x
b7 x = 1 + b6 x
b8 x = 1 + b7 x

c1 x = 1 + b8 x
c2 x = 1 + c1 x
c3 x = 1 + c2 x
c4 x = 1 + c3 x
c5 x = 1 + c4 x
c6 x = 1 + c5 x
c7 x = 1 + c6 x
c8 x = 1 + c7 x

d1 x = 1 + c8 x
d2 x = 1 + d1 x
d3 x = 1 + d2 x
d4 x = 1 + d3 x
d5 x = 1 + d4 x
d6 x = 1 + d5 x
d7 x = 1 + d6 x
d8 x = 1 + d7 x

e1 x = 1 + d8 x
e2 x = 1 + e1 x
e3 x = 1 + e2 x
e4 x = 1 + e3 x
e5 x = 1 + e4 x
e6 x = 1 + e5 x
e7 x = 1 + e6 x
e8 x = 1 + e7 x

f1 x = 1 + e8 x
f2 x = 1 + f1 x
f3 x = 1 + f2 x
f4 x = 1 + f3 x
f5 x = 1 + f4 x
f6 x = 1 + f5 x
f7 x = 1 + f6 x
f8 x = 1 + f7 x

g1 x = 1 + f8 x
g2 x = 1 + g1 x
g3 x = 1 + g2 x
g4 x = 1 + g3 x
g5 x = 1 + g4 x
g6 x = 1 + g5 x
g7 x = 1 + g6 x
g8 x = 1 + g7 x

h1 x = 1 + g8 x
h2 x = 1 + h1 x
h3 x = 1 + h2 x
h4 x = 1 + h3 x
h5 x = 1 + h4 x
h6 x = 1 + h5 x
h7 x = 1 + h6 x
h8 x = 1 + h7 x

k1 x = 1 + h8 x
k2 x = 1 + k1 x
k3 x = 1 + k2 x
k4 x = 1 + k3 x
k5 x = 1 + k4 x
k6 x = 1 + k5 x
k7 x = 1 + k6 x
k8 x = 1 + k7 x


main = print (a1 3)