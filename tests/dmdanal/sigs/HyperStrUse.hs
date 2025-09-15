module HyperStrUse where

f :: (Int, Int) -> Bool -> Int
f (x,y) True = error (show x)
f (x,y) False = x +1
