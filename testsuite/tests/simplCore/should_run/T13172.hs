module Main where

f :: Word -> Bool
f n = case n+1 of
       0 -> True
       _ -> False
{-# NOINLINE f #-}

main = do
   putStrLn "Word: wrap (0-1)"
   print (f (-1))
