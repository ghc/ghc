module F5 where

-- result not satisfiable
f5g h z = (h z) + 1
f5h f x g = f x + f5g g x -- + (f (x+1))
f5y = (\y -> y+1)
f5 = f5h f5y 0 f5y