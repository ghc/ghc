{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module T25185 where

f :: Int -> Int
f x = x

g :: Int %1 -> Int
g y = f y
