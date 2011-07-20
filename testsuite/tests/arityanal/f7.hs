module F7 where

f7f = \x -> x
f7g = \z -> \y -> z+y 
f7 = f7f f7g 2 3
