module Main where

my_id :: a -> a
my_id x = $( [| x |] )

main = print (my_id "hello")
