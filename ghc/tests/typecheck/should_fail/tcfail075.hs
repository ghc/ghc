--!! Test top-level unboxed types

{-# OPTIONS -fglasgow-exts #-}

module Main where

x = 1#

y :: Int#
y = x +# 1#

main =  let 
	  z = x -# y
	in
	if z ># 3# then putStrLn "Yes"
		   else putStrLn "No"
