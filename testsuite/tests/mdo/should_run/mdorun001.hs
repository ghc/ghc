{-# OPTIONS -XRecursiveDo -XScopedTypeVariables #-}

module Main(main) where

import Control.Monad.Fix
import Data.Array.IO
import Control.Monad

norm a = mdo (_, sz) <- getBounds a
	     s <- ioaA 1 s sz 0
	     return ()
    where 
    	  ioaA i s sz acc
	   | i > sz = return acc
	   | True   = do v <- readArray a i
	                 writeArray a i (v / s)
		         ioaA (i+1) s sz $! (v + acc)

toList a = do (_, sz) <- getBounds a
	      mapM (\i -> readArray a i) [1..sz]

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
