module PreludeNum (

	double,	compute1, compute2

    ) where

{- Preliminaries ... -}

{- patError# { Int# } (built into compiler) -}
local_map f []     = []
local_map f (x:xs) = (f x) : local_map f xs


{- Here we go ... -}

instance  Eq Int#  where
    x == y = eqInt# x y
    x /= y = neInt# x y

instance  Read Int#  where
    readsPrec p s = map (\ (I# i#, s) -> (i#, s)) (readsPrec p s)
    readList s = map (\ (x, s) -> (local_map (\ (I# i#) -> i#) x, s)) (readList s)

instance  Show Int#  where
    showsPrec p x = showsPrec p (I# x)
    showList l = showList (local_map I# l)

instance  Num Int#  where
    (+)	   x y =  plusInt# x y
    (-)	   x y =  minusInt# x y
    negate x   =  negateInt# x
    (*)	   x y =  timesInt# x y
    abs    n   = if n `geInt#` 0# then n else (negateInt# n)

    signum n | n `ltInt#` 0# = negateInt# 1#
	     | n `eqInt#` 0# = 0#
	     | otherwise     = 1#

    fromInteger (J# a# s# d#)
      = integer2Int# a# s# d#

    fromInt (I# i#) = i#

double x = x * x + x * x - x * x + x * x - x * x

compute1 n = 1# + double n
compute2 n = (1::Int) + double n



