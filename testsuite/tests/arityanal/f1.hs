module F1 where

f1 = let h1 n x = if x<n then let v = h1 n (x+1)
		              in \y -> v (x+y)
		         else \y -> y
     in h1 5 1 5

g = \x1-> \x2-> \x3-> \x4-> \x5-> x1+x2+x3+x4+x5
s f = f 3
h = s g 6 7 8