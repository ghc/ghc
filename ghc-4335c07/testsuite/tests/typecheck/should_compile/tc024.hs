module ShouldSucceed where

main x = s k k x

s f g x = f x (g x)

k x y = x
