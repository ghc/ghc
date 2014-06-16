module Arithmetic where

import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe
import Utilities
import Converter
import Stream
import Data.Ratio
import Trit


-- Negate a stream of Gray code
negateGray :: Gray -> Gray
negateGray = fl


-- Multiply a Gray code stream by 2
-- The stream must represent a real number in (-1/2, 1/2) only
mul2 :: Gray -> Gray
mul2 (x:1:xs) = (x:fl xs)


-- Division by 2, the result is to be in (-1/2, 1/2)
div2 :: Gray -> Gray
div2 (x:xs) = x:1:(fl xs)


-- Addition by 1, the input must be in (-1,0)
plusOne :: Gray -> Gray
plusOne (0:xs) = 1:fl xs



-- Substraction by 1, the input must be in (0,1)
minusOne :: Gray -> Gray
minusOne (1:xs) = 0:fl xs




threadTesting :: Gray -> Gray -> IO Int
threadTesting xs ys = do
		    m <- newEmptyMVar
		    c1 <- forkIO (t1 m xs ys)
		    c2 <- forkIO (t2 m xs ys)
		    c3 <- forkIO (t3 m xs ys)
		    c4 <- forkIO (t4 m xs ys)
		    c5 <- forkIO (t5 m xs ys)
		    c6 <- forkIO (t6 m xs ys)
		    c <- takeMVar m
		    killThread c1
		    killThread c2
		    killThread c3
		    killThread c4
		    killThread c5
		    killThread c6
		    return c




addition :: Gray -> Gray -> IO Gray
addition xs ys = do
		     			c <- threadTesting xs ys
		     			case c of
		     			   1 -> do
					           let tx = tail xs
						   let ty = tail ys
				  		   t <- unsafeInterleaveIO (addition  tx ty)
						   return (0:t)
					   2 ->	 do
						   let tx = tail xs
						   let ty = tail ys
					   	   t <- unsafeInterleaveIO (addition tx ty)
						   return (1:t)
					   3 ->  do	
						   let tx = tail xs
						   let ty = tail ys					   
					   	   cs <- unsafeInterleaveIO (addition tx (fl ty))
						   let c1 = cs !! 0
						   let c2 = tail cs
						   return (c1:1:fl c2)
					   4 ->  do
						   let tx = tail xs
						   let ty = tail ys
					   	   (cs) <- unsafeInterleaveIO (addition (fl tx) ty)
		               			   let c1 = cs !! 0
						   let c2 = tail cs
						   return (c1:1:(fl c2))
					   5 ->  do
						   let x1 = xs!!0
						   let y1 = ys!!0
						   let tx = (drop 2) xs
						   let ty = (drop 2) ys
					   	   cs <- unsafeInterleaveIO (addition (x1:(fl tx)) (y1:(fl ty)))
						   let c1 = cs !! 0
						   let c2 = tail cs
						   return (c1:(1:(fl c2)))
					   6 ->  do						   
						   let x1 = xs !! 0
						   let tx = drop 3 xs
						   let ty = drop 2 ys
					   	   t <- unsafeInterleaveIO (addition (x1:1:tx) (1:fl ty))
						   return (0:t)
					   7 ->  do
						   let x1 = xs !! 0
						   let tx = drop 3 xs
						   let ty = drop 2 ys
					   	   t <- unsafeInterleaveIO (addition (fl (x1:1:tx)) (1:(fl ty)))
						   return (1:t)
					   8 ->  do
						   let x1 = xs !! 0
						   let y2 = ys !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
					   	   t <- unsafeInterleaveIO (addition (fl (x1:fl tx)) (fl (y2:fl ty)))
						   return (0:1:t)
					   9 ->  do
						   let x1 = xs !! 0
						   let y2 = ys !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
					   	   t <- unsafeInterleaveIO (addition (x1:fl tx) (fl (y2:fl ty)))
						   return (1:1:t)
					   10 ->  do
						   let y1 = ys !! 0
						   let ty = drop 3 ys
						   let tx = drop 2 xs
					  	   t <- unsafeInterleaveIO (addition (1:fl tx) (y1:1:ty))
						   return (0:t)
					   11 ->  do
						   let y1 = ys !! 0
						   let ty = drop 3 ys
						   let tx = drop 2 xs
					   	   t <- unsafeInterleaveIO (addition (1:fl tx) (fl (y1:1:ty)))
						   return (1:t)
				 	   12 ->  do
						   let y1 = ys !! 0
						   let x2 = xs !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
					   	   t <- unsafeInterleaveIO (addition (fl (x2:fl tx)) (fl (y1:fl ty)))
						   return (0:1:t)
					   13 ->  do
						   let y1 = ys !! 0
						   let x2 = xs !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
					   	   t <- unsafeInterleaveIO (addition (fl (x2:fl tx)) (y1:fl ty))
						   return (1:1:t)













-- Compute (a-b)/2
substraction :: Gray -> Gray -> IO Gray
substraction xs ys = addition xs (negateGray ys)





t1 :: MVar Int -> Stream -> Stream -> IO()
t1 m (0:as) (0:bs) = putMVar m 1
t1 m (1:as) (1:bs) = putMVar m 2
t1 m (0:as) (1:bs) = putMVar m 3
t1 m (1:as) (0:bs) = putMVar m 4


t2 :: MVar Int -> Stream -> Stream -> IO()
t2 m (a:1:x) (b:1:y) = putMVar m 5
t2 m x y = yield
			
			
t3 m (a:1:0:x) (0:0:y) = putMVar m 6
t3 m (a:1:0:x) (1:0:y) = putMVar m 7
t3 m x y = yield
	

t4 m (a:1:0:x) (0:b:1:y) = putMVar m 8
t4 m (a:1:0:x) (1:b:1:y) = putMVar m 9
t4 m x y = yield			
			
								
t5 m (0:0:x) (b:1:0:y) = putMVar m 10
t5 m (1:0:x) (b:1:0:y) = putMVar m 11
t5 m x y = yield

			
t6 m (0:a:1:x) (b:1:0:y) = putMVar m 12
t6 m (1:a:1:x) (b:1:0:y) = putMVar m 13
t6 m x y = yield
			
			



multiplyIO :: Gray -> Gray -> IO Gray
multiplyIO xs ys = do
		    s1 <- unsafeInterleaveIO (grayToSignIO xs)
		    s2 <- unsafeInterleaveIO (grayToSignIO ys)
		    let s = Trit.multiply s1 s2
	            let g = signToGray s
                    return g

						

start :: IO()
start = do
	   c <- unsafeInterleaveIO(multiplyIO z1 z1)
	   putStrLn (show c)


startA :: IO()
startA = do
	   c <- unsafeInterleaveIO(addition (1:1:z0) (1:1:z0))
	   putStrLn (show (take 30 c))

z0 = (0:z0)
z1 = (1:z1)

zl = 0:loop:z0

loop = loop
loop01 = 0:1:loop01
