module F1 where

f1 = let h1 n x = if x<n then let v = h1 n (x+1)
		              in \y -> v (x+y)
		         else \y -> y
     in h1 5 1 5