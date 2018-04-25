
-- This tests trac #931

module Main where

x = f [1..5] (f [2..] [3..])

f xs ys = l
    where
    l = [ if s then x else y | (x, y) <- zip xs ys ]
    s = g xs ys
    g [] _ = True
    g _ [] = False
    g (x:xs) (y:ys) = g xs ys

main = print (show x)

