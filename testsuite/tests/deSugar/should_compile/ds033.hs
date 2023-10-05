-- !!! getting top-level dependencies right
--
module ShouldCompile where

f1 x = g1 x
g1 y = y

g2 y = y
f2 x = g2 x

f3 x = g3 x
g3 y = f3 y

g4 y = f4 y
f4 x = g4 x
