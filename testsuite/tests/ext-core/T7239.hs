module Main where

data T a = T a

type C = T Int
type CL = [C]

main = print 1
