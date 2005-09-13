module Trit (Trit, rationalToTrit, getIntegral, getFraction, getFraction',
		neg, addTrits, subTrits, shiftLeft, shiftRight, multiply
		) where

import Stream
import Utilities
import Data.Ratio

type Mantissa = Stream
type Fraction = Stream
type Trit     = (Mantissa, Fraction)


-- Convert from a Rational number to its Trit representation (Integral, Fraction)
rationalToTrit :: Rational -> Trit
rationalToTrit x
		|x<1		= ([0], rationalToStream x)
		|otherwise	= (u', rationalToStream v)
		where   u = n `div` d
			u' = toBinary u
	      		v = x - (toRational u)
			n = numerator x
	      		d = denominator x
			
			
-- Get the integral part of Trit
getIntegral :: Trit -> Mantissa
getIntegral = fst



-- Get the fraction part of Trit, with n digit of the stream
getFraction :: Int -> Trit -> Stream
getFraction n = take n. snd


-- Get the fraction part of Trit
getFraction' :: Trit -> Stream
getFraction' = snd



-- Negate a Trit
neg :: Trit -> Trit
neg (a, b) = (negate' a, negate' b)



-- Add two Trits
addTrits :: Trit -> Trit -> Trit
addTrits (m1, (x1:x2:xs)) (m2, (y1:y2:ys)) = (u,addStream (x1:x2:xs) (y1:y2:ys))
					   where u' = addFiniteStream m1 m2
					         c = [carry x1 x2 y1 y2]
						 u = addFiniteStream u' c



-- Substraction of 2 Trits
subTrits :: Trit -> Trit -> Trit
subTrits x y = addTrits x (neg y)



-- Shift left = *2 opertaion with Trit
shiftLeft :: Trit -> Trit
shiftLeft (x, (y:ys)) = (x++ [y], ys)


-- Shift right = /2 operation with Trit
shiftRight :: Trit -> Integer -> Trit
shiftRight (x, xs) 1 = (init x, (u:xs))
		    where u = last x
shiftRight (x, xs) (n+1) = shiftRight (init x, (u:xs)) n
 		    where u = last x		    
		  
		  

-- Multiply a Trit stream by 1,0 or -1, simply return the stream
mulOneDigit :: Integer -> Stream -> Stream
mulOneDigit x xs
              |x==1      = xs
	      |x==0      = zero'
	      |otherwise = negate' xs
	      where zero' = (0:zero')






-- Multiplication of two streams
multiply :: Stream -> Stream -> Stream
multiply (a0:a1:x) (b0:b1:y) = average p q
			       where p = average (a1*b0: (average (mulOneDigit b1 x)
							         (mulOneDigit a1 y)))
					         (average (mulOneDigit b0 x)
						          (mulOneDigit a0 y))
				     q = (a0*b0:a0*b1:a1*b1:(multiply x y))




start0 = take 30 (multiply (rationalToStream (1%2)) zo)

zo :: Stream
zo = 1:(-1):zero
     where zero = 0:zero

start1 = take 30 (average (rationalToStream (1%2)) (negate' (rationalToStream (1%4))))

	 
	 
