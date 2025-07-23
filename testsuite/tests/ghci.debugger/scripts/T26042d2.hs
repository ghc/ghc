
module Main where

main = do
  putStrLn "hello1"
  f
  putStrLn "hello3"
  putStrLn "hello4"

f = do
  putStrLn "hello2.1"
  putStrLn "hello2.2"
{-# NOINLINE f #-}
