{-# OPTIONS -fglasgow-exts #-}

module Main where

import Foreign
import Stable

-- simple test for building/dereferencing stable ptrs

main 
  = do	l <- mapM makeStablePtr [1..100000]
   	sum <- stable_sum l
   	print sum

stable_sum :: [StablePtr Integer] -> IO Integer
stable_sum [] = return 0
stable_sum (x:xs) 
  = do 	x'  <- deRefStablePtr x
	freeStablePtr x
       	xs' <- stable_sum xs
        return (x' + xs')
