















{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}

module PrelNum where

import {-# SOURCE #-} PrelErr
import PrelBase
import PrelList
import PrelEnum
import PrelShow

infixl 7  *
infixl 6  +, -

default ()		-- Double isn't available yet, 
			-- and we shouldn't be using defaults anyway









class  (Eq a, Show a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a -- partain: Glasgow extension

    x - y		= x + negate y
    negate x		= 0 - x
    fromInt (I# i#)	= fromInteger (S# i#)
					-- Go via the standard class-op if the
					-- non-standard one ain't provided





subtract	:: (Num a) => a -> a -> a
{-# INLINE subtract #-}
subtract x y	=  y - x

ord_0 :: Num a => a
ord_0 = fromInt (ord '0')










instance  Num Int  where
    (+)	   x y =  plusInt x y
    (-)	   x y =  minusInt x y
    negate x   =  negateInt x
    (*)	   x y =  timesInt x y
    abs    n   = if n `geInt` 0 then n else (negateInt n)

    signum n | n `ltInt` 0 = negateInt 1
	     | n `eqInt` 0 = 0
	     | otherwise   = 1

    fromInt n	  = n




-- These can't go in PrelBase with the defn of Int, because
-- we don't have pairs defined at that time!

quotRemInt :: Int -> Int -> (Int, Int)
a@(I# _) `quotRemInt` b@(I# _) = (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

divModInt ::  Int -> Int -> (Int, Int)
divModInt x@(I# _) y@(I# _) = (x `divInt` y, x `modInt` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)










data Integer	
   = S# Int#				-- small integers
   | J# Int# ByteArray#			-- large integers





zeroInteger :: Integer
zeroInteger = S# 0#

