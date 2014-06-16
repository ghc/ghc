{-# LANGUAGE UnboxedTuples #-}

-- O2 to get CSE

module Main where

f :: Int -> (# Int, Int #)
f 0 = (# 1,2 #)
f n = f (n-1)

{-# NOINLINE g #-}
g x = case f x of 
	(# a,b #) -> if a>0 
		     then f x 	-- CSE opportunity
		     else (# b,a #)

-- GHC 6.2 wrongly optimised g to:
--	case f x of t 
--	  (# a,b #) -> if a>0 then
--			  t	-- WRONG
--			else (# b,a #)

main = case g 2 of (# a,b #) -> print a
