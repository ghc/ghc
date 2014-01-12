module Stream (Stream, carry, addStream, rationalToStream, 
		streamToFloat, addFiniteStream, negate', average) where

import Data.Ratio


type Digit = Integer
type Stream = [Integer]



-- Convert from a Rational fraction to its stream representation
rationalToStream :: Rational -> Stream
rationalToStream x
	|t<1		= 0:rationalToStream t
	|otherwise	= 1:rationalToStream (t-1)
	where t = 2*x
	
	

	
-- Convert from a stream to the Float value
streamToFloat :: Stream -> Float
streamToFloat x =  f x (1)

f :: Stream -> Integer -> Float
f [] n = 0
f (y:ys) n = (fromIntegral)y/(fromIntegral(2^n)) + f ys (n+1)	





-- Add two stream 
addStream :: Stream -> Stream -> Stream
addStream (x1:x2:x3:xs) (y1:y2:y3:ys) = (u+c):(addStream (x2:x3:xs) (y2:y3:ys))
				where u = interim x1 x2 y1 y2
				      c = carry x2 x3 y2 y3
				      
				      
				      
-- Compute carry, the C(i) value, given x(i) and y(i)
carry :: Digit -> Digit -> Digit -> Digit -> Digit
carry x1 x2 y1 y2 
	|t>1			=  1
	|t<(-1)			= -1
	|t==1 && (minus1 x2 y2)	=  0
	|t==1 && not (minus1 x2 y2) = 1
	|t==(-1) && (minus1 x2 y2) = -1
	|t==(-1) && not (minus1 x2 y2) = 0
	|t==0				= 0
	where t = x1+y1
	
	
	
-- Computer the interim sum, the U(i) value, given x(i), y(i) and c(i)
interim :: Digit -> Digit -> Digit -> Digit -> Digit
interim x1 x2 y1 y2
		|t>1			=  0
		|t<(-1)			= 0
		|t==1 && (minus1 x2 y2)	=  1
		|t==1 && not (minus1 x2 y2) = -1
		|t==(-1) && (minus1 x2 y2) = 1
		|t==(-1) && not (minus1 x2 y2) = -1
		|t==0				= 0
		where t = x1+y1



-- Check if at least one of 2 digits is -1
minus1 :: Digit -> Digit -> Bool
minus1 x y = (x==(-1))|| (y==(-1))






-- Algin two stream so that they have the same length
align :: Stream -> Stream -> (Stream, Stream)
align xs ys 
	|x>y 		= (xs, (copy 0 (x-y)) ++ys)
	|otherwise      = ((copy 0 (y-x)) ++ xs, ys)
	where x = toInteger(length xs)
	      y = toInteger(length ys)
	      
	      
	      
-- Generate a list of x
copy :: Integer -> Integer -> [Integer]
copy x n = [x| i<- [1..n]]







-- Add two finite stream (to add the integral part)
addFiniteStream :: Stream -> Stream -> Stream
addFiniteStream xs ys = add' u v
			where (u,v) = align xs ys
			
			
			
-- Utility function for addFinitieStream			
add' :: Stream -> Stream -> Stream
add' u v = normalise (f u v)
       where f [] [] = []
             f (x:xs) (y:ys) = (x+y):f xs ys
	     
	     
-- Normalise the sum
normalise :: Stream -> Stream
normalise = foldr f [0]
            where f x (y:ys) = (u:v:ys)
	                      where u = (x+y) `div` 2
			            v = (x+y) `mod` 2


-- Negate a stream
negate' :: Stream -> Stream
negate' = map (*(-1))



-- Compute average of two stream
-- Using [-2,-1,0,1,2] to add, and then divide by 2
average :: Stream -> Stream -> Stream
average xs ys = div2 (add xs ys)


-- Addition of two streams, using [-2,-1,0,1,2]
add :: Stream -> Stream -> Stream
add (x:xs) (y:ys) = (x+y):(add xs ys)


-- Then divided by 2, [-2,-1,0,1,2] -> [-1,0,1]
div2 :: Stream -> Stream
div2 (2:xs)          	= 1:div2 xs 		       
div2 ((-2):xs)		= (-1):div2 xs
div2 (0:xs)		= 0:div2 xs
div2 (1:(-2):xs)	= div2 (0:0:xs)
div2 (1:(-1):xs)	= div2 (0:1:xs)
div2 (1:0:xs)		= div2 (0:2:xs)
div2 (1:1:xs)		= div2 (2:(-1):xs)
div2 (1:2:xs)		= div2 (2:0:xs)
div2 ((-1):(-2):xs)	= div2 ((-2):0:xs)
div2 ((-1):(-1):xs)	= div2 ((-2):1:xs)
div2 ((-1):0:xs)	= div2 (0:(-2):xs)
div2 ((-1):1:xs)	= div2 (0:(-1):xs)
div2 ((-1):2:xs)	= div2 (0:0:xs)
    		
 

test = take 100 (average (rationalToStream (1%2)) (rationalToStream (1%3)))
