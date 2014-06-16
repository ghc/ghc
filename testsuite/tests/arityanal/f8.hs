module F8 where

f8f b x y = let g = \z -> x+y+z
	    in if b then y else g (x*x)
f8 = f8f True 1 2