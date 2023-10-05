module Main where

{-# NOINLINE wiz #-}
wiz :: Int -> Int
wiz x = x + 10

main = do
  print (wiz 5)
  return ()
