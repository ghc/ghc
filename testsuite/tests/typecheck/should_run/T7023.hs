{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

a = ["x", 1 :: Int]

main :: IO ()
main = putStrLn "Hello World"

