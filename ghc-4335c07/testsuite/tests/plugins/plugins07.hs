module Main where

{-# NOINLINE x #-}
x = "foo"

main = putStrLn (show x)
