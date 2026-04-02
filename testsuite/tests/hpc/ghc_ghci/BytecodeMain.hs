module Main where

inc :: Int -> Int
inc x = x + 1

double :: Int -> Int
double x = x * 2

main :: IO ()
main = print (double (inc 1011))
