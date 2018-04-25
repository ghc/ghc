module Main where

f1 :: Int -> Int
f1 = (+ 1)

f2 :: Int -> Int
f2 = f3 . f3 . f3
  where
    f3 :: Int -> Int
    f3 = (* 123)
    {-# SCC f3 "bar" #-}
    {-# NOINLINE f3 #-}



main :: IO ()
main = readLn >>= print . f2 . f1

{-# NOINLINE f1 #-}
{-# NOINLINE f2 #-}

{-# SCC f1 #-}
{-# SCC f2 "foo" #-}
