{-# OPTIONS_GHC -O2 -fforce-recomp #-}

module T18937 where

f :: [Int] -> Int -> Int
f []     = id
f (x:xs) = let y = sum [0..x]
           in \z -> f xs (y + z)
