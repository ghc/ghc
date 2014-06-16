module F0 where

f0 :: Int -> Int -> Int -> Int
f0 x y = if (x>0) then let v = x + y 
		       in \z -> v+z
		  else \z-> 1