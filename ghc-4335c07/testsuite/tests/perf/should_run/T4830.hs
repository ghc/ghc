-- Compile with O2; SpecConstr should fire nicely
-- and eliminate all allocation in inner loop

module Main where

foo :: Int -> Maybe (Double,Double) -> Double
foo _ Nothing      = 0
foo 0 (Just (x,y)) = x+y
foo n (Just (x,y)) = let r = f x y in r `seq` foo (n-1) (Just r)
  where
    f x y | x <= y    = (x,y)
          | otherwise = (y,x)

main = print (foo 1000000 (Just (1,2)))

