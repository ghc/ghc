module F4 where

f4h :: (Int -> Int) -> Int -> Int
f4h f x = if x==0 then (f x)
		 else f4h f (x-1) -- + (f x)
f4g = \y->y+1
f4 = f4h f4g 9