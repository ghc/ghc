module F3 where

fac :: Int -> Int
fac x = if (x==0) then 1
		  else x*fac (x-1)

f3 = let v = fac
     in \y -> v y