module Main where

x :: ((->) Int) Bool
x = (==0)

main = print $ x 0
