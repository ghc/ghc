module ShouldSucceed where

main  = s k k 

s f g x = f x (g x)

k x y = x
