-- !!! ds015 -- lambdas
--
module Tests where

f x = ( \ x -> x ) x

g x y = ( \ x y -> y x ) ( \ x -> x ) x

h x y = ( \ (x:xs) -> x ) x
