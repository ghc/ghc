module F2 where

f2f = \h -> \x -> h x 0 
f2 = let g = \x -> \y -> if (x > 0)
				then g (x-1) (x+y)
				else y
     in f2f g 5 