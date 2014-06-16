
module Main where

import Arithmetic
import Trit
import Converter
import System.IO.Unsafe
import Data.Ratio
import Utilities
import Thread

main = startM1						  

startM1 :: IO()
startM1 = do
	  c <- unsafeInterleaveIO (mult (rationalToGray (1%3)) (rationalToGray (0%1)))
	  putStrLn (show  (take 100 (drop 1 c)))




mult :: Gray -> Gray -> IO Gray
mult xs ys = do
		     			c <- threadTesting1 xs ys
		     			case c of

					   101 -> do
						   --putStrLn ("In case 101")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t1 <- unsafeInterleaveIO (addition tx ty)
						   t2 <- unsafeInterleaveIO (addition (fl t1) (1:t1))
						   t3 <- unsafeInterleaveIO (mult tx ty)
						   c' <- unsafeInterleaveIO (addition t2 (1:0:0:(fl t3)))
						   return c'

					   102 -> do
						   --putStrLn ("In case 102")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t1 <- unsafeInterleaveIO (addition (fl tx) ty)
					           t2 <- unsafeInterleaveIO (addition tx ty)
						   t0 <- unsafeInterleaveIO (addition t1 (1:fl t2))
						   t3 <- unsafeInterleaveIO (mult tx ty)
						   c' <- unsafeInterleaveIO (addition t0 (1:1:0:fl t3))
						   return c'

					   103 -> do
						   --putStrLn ("In case 103")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t <- unsafeInterleaveIO (mult (0:0:tx) (0:0:ty))
						   return (fl t)

					   104 -> do
						   --putStrLn ("In case 104")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t <- unsafeInterleaveIO (mult (0:0:tx) (0:1:ty))
						   return (fl t)

					   201 -> do
						   c' <- unsafeInterleaveIO (mult ys xs)
						   return c'
						   
					   202 -> do
						   --putStrLn ("In case 202")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t1 <- unsafeInterleaveIO (addition tx ty)
						   t2 <- unsafeInterleaveIO (addition t1 (0:fl t1))
						   t3 <- unsafeInterleaveIO (mult tx ty)
						   c' <- unsafeInterleaveIO (addition t2 (1:1:1:fl t3))
						   return c'
						   
					   203 -> do
						   --putStrLn ("In case 203")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t <- unsafeInterleaveIO (mult (0:1:tx) (0:0:ty))
						   return (fl t)

					   204 -> do
						   --putStrLn ("In case 204")
						   let tx = drop 2 xs
						   let ty = drop 2 ys
						   t <- unsafeInterleaveIO (mult (0:1:tx) (0:1:ty))
						   return (fl t)

					   30 ->  do
						   --putStrLn ("In case 30")
						   let y1 = ys !! 0
						   let tx = drop 2 xs
						   let ty = drop 3 ys
					           t1 <- unsafeInterleaveIO (addition ((f0' y1):1:ty) ((f0' y1):1:0:ty))
						   t0 <- unsafeInterleaveIO (mult tx (y1: fl ty))
						   let c4 = head t0
						   let d4 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition t1 (c4:1:0:0:d4))
						   return c'
						   
					   31 -> do
					   	  --putStrLn ("In case 31")
					   	  let tx = drop 2 xs
						  c' <- unsafeInterleaveIO (mult (0:0:tx) ys)
						  return (fl c')
						  
					   40 ->  do
					   	   --putStrLn ("In case 40")
						   let tx = drop 2 xs
						   let y2 = ys !! 1
						   let ty = drop 3 ys
						   t1 <- unsafeInterleaveIO (addition (y2:fl ty) tx)
						   t2 <- unsafeInterleaveIO (addition (fl t1) (1:y2:1:ty))
						   t0 <- unsafeInterleaveIO (mult tx (y2:fl ty))
						   let c2 = f0' (head t0)
						   let d2 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition t2 (1:c2:1:0:d2))
						   return c'
						   				   
					   41 ->  do
					   	   --putStrLn ("In case 41")
					   	   let tx = drop 2 xs
						   let y2 = ys !! 1
						   let ty = drop 3 ys
						   c' <- unsafeInterleaveIO (mult (0:0:tx) (0:y2:1:ty))
						   return (fl c')
						   
					   50 ->  do
					           --putStrLn ("In case 50")
					   	   let tx = drop 2 xs
						   let y2 = ys !! 1
						   let ty = drop 3 ys
						   t1 <- unsafeInterleaveIO (addition tx (fl (y2:fl ty)))
						   t2 <- unsafeInterleaveIO (addition t1 (0:y2:1:ty))
						   t0 <- unsafeInterleaveIO (mult (fl tx) (y2:fl ty))
						   let c1 = f0' (head t0)
						   let d1 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition t2 (1:c1:1:0:d1))
						   return c'
						   
					   51 ->  do
					           --putStrLn ("In case 51")
					   	   let tx = drop 2 xs
						   let y2 = ys !! 1
						   let ty = drop 3 ys
						   c' <- unsafeInterleaveIO (mult (0:1:tx) (0:y2:1:ty))
						   return (fl c')
						   
						   
					   60 ->  do
					           --putStrLn ("In case 60")
					   	   let tx = drop 2 xs
						   let y1 = ys !! 0
						   let ty = drop 3 ys
						   t1 <- unsafeInterleaveIO (addition ((f0' y1):1:ty) (y1:1:0:ty))
						   t0 <- unsafeInterleaveIO (mult (fl tx) (y1:fl ty))
						   let c1 = head t0
						   let d1 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition t1 (c1:1:0:0:d1))
						   return c'
						   
					   61 ->  do
					           --putStrLn ("In case 61")
					   	   let tx = drop 2 xs
						   let y1 = ys !! 0
						   let ty = drop 3 ys
						   c' <- unsafeInterleaveIO (mult (0:1:tx) (y1:1:0:ty))
						   return (fl c')
						   
						   
					   70 ->  do
					           --putStrLn ("In case 70")
					   	   c' <- unsafeInterleaveIO (mult ys xs)
						   return c'
						   
					   80 ->  do
					           --putStrLn ("In case 80")
						   let x2 = xs !! 1
						   let y2 = ys !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
						   t1 <- unsafeInterleaveIO (addition (x2:fl tx) (y2:fl ty))
						   t0 <- unsafeInterleaveIO (mult (x2:fl tx) (y2:fl ty))
						   let c1 = head (fl t1)
						   let d1 = tail (fl t1)
						   let c2 = f0' (head t0)
						   let d2 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition (c1:1:(fl d1)) (1:c2:1:0:d2))
						   return c'
						   
					   81 ->  do
					           --putStrLn ("In case 81")
					   	   let x2 = xs !! 1
						   let y2 = ys !! 1
						   let tx = drop 3 xs
						   let ty = drop 3 ys
						   c' <- unsafeInterleaveIO (mult (0:x2:1:tx) (0:y2:1:ty))
						   return (fl c')
						   
					   90 ->  do
					   	   --putStrLn ("In case 90")
						   let x2 = xs!!1
						   let y1 = ys!!0
						   let tx = drop 3 xs
						   let ty = drop 3 ys
						   t0 <- unsafeInterleaveIO (mult (x2:fl tx) (y1:fl ty))
						   let c1 = head t0
						   let d1 = fl (tail t0)
						   c' <- unsafeInterleaveIO (addition ((f0' y1):1:0:ty) (c1:1:0:0:d1))
						   return c'
						   
					   91 ->  do
					   	   --putStrLn ("In case 91")
					   	   let x2 = xs!!1
						   let y1 = ys!!0
						   let tx = drop 3 xs
						   let ty = drop 3 ys
						   c' <- unsafeInterleaveIO (mult (0:x2:1:tx) (y1:1:0:ty))
						   return (fl c')
						   
					   100 -> do
					   	  --putStrLn ("In case 100")
					          let x1 = head xs
						  let y1 = head ys
						  let tx = drop 3 xs
						  let ty = drop 3 ys
						  t0 <- unsafeInterleaveIO (mult (x1:fl tx) (y1:fl ty))
						  let c4 = head t0
						  let d4 = fl (tail t0)
						  return (c4:1:0:0:0:d4)
						  
						   
				
f0' 0 = 1
f0' 1 = 0						   
						   
