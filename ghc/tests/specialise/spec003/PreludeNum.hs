module PreludeNum (f1, f2, fac, f3, fac_two) where

{- Preliminaries ... -}

{- patError# { Int# } (built into compiler) -}
local_map f []     = []
local_map f (x:xs) = (f x) : local_map f xs

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


first  (a, b) = a
second (a, b) = b

{- Here we go ... -}

fac 0 = 1
fac n = n * (fac (n - 1))

{-# INLINE f1 #-}
f1 = fac 10#

f2 = f1 * f1

fac_two n two = case n of 0 -> (1, 1)
		          n -> (n * (first (fac_two (n - 1) two)), 2)

f3 = let (res1, two1) = fac_two (10::Int#) (two2::Int#)
	 (res2, two2) = fac_two (10::Int)  (two1::Int)
     in
     res1 + two2

