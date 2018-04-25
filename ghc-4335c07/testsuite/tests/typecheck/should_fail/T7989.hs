module T7989 where

data A = A {a0, a1 :: Int}
data B = B {b0, b1 :: Int}

f x = x { a0 = 3, a1 = 2, b0 = 4, b1 = 5 }

data T = T1 { x,y,v :: Int} | T2 { y,z,v :: Int } | T3 { z,x,v :: Int}
g a = a { x=0, y=0, z=0, v=0 }

h a = a { x=0, a0=0 }