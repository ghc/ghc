-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Rational(
radd, rsub, rmul, rdiv, rmin, rabs, intval, show_rat, torat) where
--CR removed norm from export list, renamed rmin2 as rmin

norm :: (Int, Int) -> (Int, Int)
norm (x,y) = (u `div` d, v `div` d)
             where
             u = if y > 0 then x else -x
	     v = abs y
             d = gcd (abs u) v

	     gcd :: Int -> Int -> Int
	     gcd 0 n2  = n2
	     gcd n1 0  = n1
	     gcd n1 n2 = if n1 < n2 then gcd n1 (n2 `mod` n1) else gcd (n1 `mod` n2) n2

radd, rsub, rmul, rdiv :: (Int, Int) -> (Int, Int) -> (Int, Int)
radd (x,y) (u,v) = norm (x * v + u * y, y * v)
rsub (x,y) (u,v) = norm (x * v - u * y, y * v)
rmul (x,y) (u,v) = norm (x * u, y * v)
rdiv (x,y) (u,v) = norm (x * v, y * u)

--CR removed unnecessary normalisation, renamed as rmin
rmin (x,y) (u,v) = if a > 0 then (u,v) else (x,y)
                   where (a,b) = rsub (x,y) (u,v)

--CR removed unnecessary normalisation
rabs :: (Int, Int) -> (Int, Int)
rabs (x,y) = if x<0 then (-x,y) else (x,y)

-- if y is zero we have an error condition
intval :: (Int, Int) -> Int
intval (x,y) = x `div` y

--for debugging
show_rat :: (Int, Int) -> [Char]
show_rat (x,y) = show x ++ "/" ++ show y  

torat :: Int -> (Int, Int)
torat n = (n,1)



