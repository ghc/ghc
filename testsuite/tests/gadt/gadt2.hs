{-# LANGUAGE ExplicitForAll,GADTs #-}

-- Pattern match uses dictionaries bound higher up in the pattern

module Main where

data T = forall a. Integral a => T a

f :: T -> Bool
f (T 0) = True
f (T n) = False

g :: T -> Ordering
g (T n) | n >= 3 = if n>3 then GT else EQ
g (T n)     = LT

main = do   print [f (T 0), f (T 1)]
	    print [g (T 2), g (T 3), g (T 4)]
