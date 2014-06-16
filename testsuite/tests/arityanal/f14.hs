module F14 where

f14 n x = if x<n then let v = f14 n (x+1)
                      in \y -> v (x+y)
               else \y -> y
