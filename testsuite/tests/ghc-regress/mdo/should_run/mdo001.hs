{-# OPTIONS -fglasgow-exts #-}

module Main(main) where

import Control.Monad.Fix
import Data.Array.IO
import Monad

norm a = mdo s <- ioaA 1 s 0
	     return ()
    where (_, sz) = bounds a
    	  ioaA i s acc
	   | i > sz = return acc
	   | True   = do v <- readArray a i
	                 writeArray a i (v / s)
		         ioaA (i+1) s $! (v + acc)

toList a = mapM (\i -> readArray a i) [1..sz]
     where (_, sz) = bounds a

test    :: Int -> IO ()
test sz = do
	(arr :: IOArray Int Float) <- newArray (1, sz) 12
	putStrLn "Before: "
	toList arr >>= print 
	norm arr
	putStrLn "After: "
	lst <- toList arr 
	print lst
	putStrLn ("Normalized sum: " ++ show (sum lst))

main = test 10
