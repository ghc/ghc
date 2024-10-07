module Main where

import Data.Bits

{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = 1 + popCount x

main :: IO ()
main = print (foo 42)
