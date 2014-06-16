module C (f, g, h) where

import D
 
g x = f x + 1

h x = x `div` 2

data C = C {x :: Int}
