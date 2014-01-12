module F6 where

f6f = \h -> \x -> h x 0
f6t = \y -> \z -> y + z
f6 = f6f f6t 3	