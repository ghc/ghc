module Main where

fun :: IO ()
fun = pure ()
{-# noinline fun #-}

{-# rules "fun" fun = putStrLn "fun" #-}

main :: IO ()
main = fun
