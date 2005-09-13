
module Converter (rationalToGray, grayToSignIO, signToGray, Gray, startF, startC) where

import Stream
import Data.Ratio
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe


type Gray = [Integer]
type State = (Integer, Integer)


-- Convert a rational number (in (-1,1)) to its Gray representation
rationalToGray :: Rational -> Gray
rationalToGray x
		|x<0		= f (negate' (rationalToStream (-x))) (0,0)
		|otherwise	= f (rationalToStream x) (0,0)
		
	
-- Function to implement the two heads Turing machine that convert a
-- signed-digit stream to the corresponding Gray-code representation		
f :: Stream -> State -> Stream
f (x:xs) (0,0)
	    |x==(-1)	= 0:f xs (0,0)
	    |x==0	= c:1:ds
	    |x==1	= 1:f xs (1,0)
	    where c:ds = f xs (0,1)
	    
f (x:xs) (0,1)
	    |x==(-1)	= 0:f xs (1,0)
	    |x==0	= c:0:ds
	    |x==1	= 1:f xs (0,0)
	    where c:ds = f xs (0,1)
	    
f (x:xs) (1,0)
	    |x==(-1)	= 1:f xs (0,0)
	    |x==0	= c:1:ds
	    |x==1	= 0:f xs (1,0)
	    where c:ds = f xs (1,1)
	    
f (x:xs) (1,1)
	    |x==(-1)	= 1:f xs (1,0)
	    |x==0	= c:0:ds
	    |x==1	= 0:f xs (0,0)
	    where c:ds = f xs (1,1)
	  
	  
	  

-- Anotherway to convert from a rational to Gray code representation
-- Behave exactly the same like above	  
rationalToGray' :: Rational -> Gray
rationalToGray' x
		|x<0		= signToGray (negate' (rationalToStream (-x)))	  
	  	|otherwise	= signToGray (rationalToStream x) 
		

-- Function to convert a signed-digit stream to Gray representation
-- Is much shorter than above		
signToGray :: Stream -> Stream
signToGray (1:xs) 	= 1:f'(signToGray xs)
signToGray ((-1):xs) 	= 0:signToGray xs
signToGray (0:xs) 	= c:1:(f' ds)
    		  	where c:ds = signToGray xs
	    


-- Convert a Gray-code stream to the corresponding signed-digit representation
-- Make use of threads 	    
grayToSignIO :: Stream -> IO Stream
grayToSignIO (x1:x2:xs) = do
			c <- threadTesting(x1:x2:xs)
			if (c==1)
			   then (do  co <- unsafeInterleaveIO (grayToSignIO (f'(x2:xs)))
			             return (1:co))
			   else if (c==2) 
			   	   then (do co <- unsafeInterleaveIO (grayToSignIO (x2:xs))
				            return ((-1):co))
                                   else (do co <- unsafeInterleaveIO (grayToSignIO (x1:f' xs))
				            return (0:co))

-- Flip the first bit of an infinite stream
f' (x:xs) = (f'' x):xs
	    where f'' 1 = 0
	    	  f'' 0 = 1


-- Launch two threads which run concurrently, test for the first digit of the stream (1, 0 or bottom)
-- As soon as one thread terminate, grab that result and proceed
threadTesting :: Stream -> IO Int
threadTesting xs = do   m <- newEmptyMVar
	    		c1 <- forkIO (f1 m xs)
		    	c2 <- forkIO (f2 m xs)
	    		c <- takeMVar m
	    		killThread c1
	    		killThread c2
	    		return c
	    
-- Test case 1, when the first bit is either 1 or 0. 
-- In case of bottom, f1 will never terminate, then f2 will definitely terminate	    
f1 :: MVar Int -> Stream -> IO() 
f1 m (0:xs) = putMVar m 2
f1 m (1:xs) = putMVar m 1

-- Test case 2, when the first bit is completely ignored, esp in case it was a bottom
-- If the second bit is 1, then we can output, don't care value of the first bit
-- If the second bit is 0, then loop forever, give chances to f1 to terminate
f2 :: MVar Int -> Stream -> IO()
f2 m (c1:c2:xs) 
		|c2==1		= putMVar m 3
		|otherwise	= yield




-- Testing
startC :: IO()
startC = do
	    c<- unsafeInterleaveIO (grayToSignIO (1:1:z0))
	    putStrLn (show (take 100 c))


startF = signToGray ((-1):1:z0)


z0 = 0:z0
loop' = loop'
z1' = (1:z1')
