module Main(main) where

{-# NOINLINE doStuff #-}
doStuff x = do
    print x
    return x

{-# NOINLINE main #-}
main :: IO Int
main = doStuff 42


