{-# LANGUAGE UnboxedSums, MagicHash #-}

module Main where

type Ty = (# (Int -> Int) | (Int -> Int) #)

{-# NOINLINE apply #-}
apply :: Ty -> Int
apply (# f | #) = f 0
apply (# | f #) = f 1

main :: IO ()
main = do
  print (apply (# (\x -> x * 2) | #))
  print (apply (# | (\x -> x * 3) #))
