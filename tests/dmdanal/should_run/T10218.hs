{-# OPTIONS_GHC -feager-blackholing #-}

module Main where

{-# NOINLINE foo #-}
foo :: Bool -> Int -> Int -> Int
foo True  _ x = 1
foo False _ x = x+1

{-# NOINLINE bar #-}
bar :: Int -> (Int,Int)
bar x = let y1 = x * 2
            y2 = x * 2
        in (foo False y1 y2,foo False y2 y1)

main = print (fst p + snd p)
  where
    p = bar 3
