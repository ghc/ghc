{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- Pattern-match overlap warnings with view patterns
module T2395 where

foo :: Int -> Int
foo (even -> True) = 4
foo _              = 5

bar :: (a, (Int,Int)) -> Int
bar (snd -> (x,y)) = x+y   -- Cannot fail, hence overlap warning should 
bar _              = 6     -- for second pattern
