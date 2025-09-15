module Foo where

f True (x,y,z,_,_) b1 b2 b3 b4 b5 b6 b7 b8 = (x,y,z, b1, b2, b3, b4, b5, b6, b7, b8)
f False at         b1 b2 b3 b4 b5 b6 b7 b8 = f True at b1 b2 b3 b4 b5 b6 b7 b8


g True (x,y,z,_,_) b1 b2 b3 = (x,y,z, b1, b2, b3)
g False at         b1 b2 b3 = g True at b1 b2 b3
