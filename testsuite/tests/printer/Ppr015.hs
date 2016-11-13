module ExprPragmas where

a = {-# SCC "name"   #-}  0x5

b = {-# SCC foo   #-} 006

c = {-# GENERATED "foobar" 1 : 2  -  3 :   4 #-} 0.00
