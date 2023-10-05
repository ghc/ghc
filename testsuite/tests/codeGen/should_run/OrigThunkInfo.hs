module Main where
xs = iterate (+1) 0
ten = xs !! 10
main = print ten
