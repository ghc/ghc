MODIFICATIONS
-------------
07-04-94   ipoole  added pi method for Lognum

SCCS: %W% %G%

A packeage for log representations of numbers.

> module Lognum where
> import Lib

> data Lognum = LN Double

> instance Num Lognum where
>	LN x * LN y = LN (x+y)
>	LN x + LN y = LN (d+(mylog 1 (exp (x-d) + exp (y-d))))
>	              where d = max x y
>	LN x - LN y = if y > x then error "subtract LN" else
>		     LN (d+(mylog 2 (exp (x-d) - exp (y-d))))
>	              where d = x
>	fromInteger 0 = LN (-1.0e99)
>	fromInteger x = LN (mylog 3 (fromInteger x))

> instance Ord Lognum where
>	LN x > LN y = x > y
>       a <= b = not  (a > b)

> instance Eq Lognum where
>	LN x == LN y = x == y

> instance Floating Lognum where
>	sqrt (LN x) = LN (x/2.0)
>	(LN x) ** (LN y) = (LN (x * exp y))
>       pi = (LN (log pi))

> instance Fractional Lognum where
>	LN x / LN y = LN (x-y)
>	fromRational x = if x == toRational 0.0 then fromInteger 0 
>			 else LN (mylog 4 (toDouble x))


> instance Enum Lognum where
>    enumFrom n       = iterate ((fromRational 1.0)+) n
>    enumFromThen n m = iterate ((m-n)+) n


> instance Real Lognum where
> 	toRational (LN x) = toRational (exp x)

> toLognum :: Real a => a -> Lognum
> toLognum = fromRational . toRational

> instance RealFloat Lognum

> instance RealFrac Lognum

> instance Show{-was:Text-} Lognum 

> mylog :: Int -> Double -> Double
> mylog n x = if toDouble x < 0.0 then error ("mylog" ++ show n) else log x



