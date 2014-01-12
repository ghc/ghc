module ShouldSucceed where

g x = f (f True x) x 
f x y = if x then y else (f x y)
