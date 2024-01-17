module T24327A where

{-# INLINE foo1 #-}
foo1 :: Char -> Int -> Int
foo1 _  y = bar1 y

{-# INLINE bar1 #-}
bar1 :: Int -> Int
bar1 x = length [1..10] + x

