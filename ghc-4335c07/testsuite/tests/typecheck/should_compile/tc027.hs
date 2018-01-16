module ShouldSucceed where

h x = f (f True x) x 
f x y = if x then y else (g y x)
g y x = if x then y else (f x y)
