module LocToName where

{-








-}







LocToName.newPoint (x:xs) = x ^2 + LocToName.newPoint xs
    -- where sq x = x ^pow 
    --       pow = 2

LocToName.newPoint [] = 0

