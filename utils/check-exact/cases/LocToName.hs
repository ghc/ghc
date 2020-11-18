module LocToName where

{-








-}







sumSquares (x:xs) = x ^2 + sumSquares xs
    -- where sq x = x ^pow 
    --       pow = 2

sumSquares [] = 0

