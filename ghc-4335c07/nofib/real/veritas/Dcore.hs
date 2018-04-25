module Dcore ( int_to_str , list_to_str , str_to_int , str_to_list
		, encode )
	      where

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

import Core_datatype

int_to_str :: Int -> String

int_to_str i 
	| i < 256   = [ toEnum 0 , toEnum i ]
	| i < 65536 = [] -- [ toEnum ((i `div` 256) `rem` 256) , toEnum (i `rem` 256) ]
	| otherwise = error "Bind" -- ** exn





list_to_str obj_to_str_fn objL 
	= int_to_str (length  objL) ++ concat (map obj_to_str_fn objL)




str_to_int :: [Int] -> (Int, [Int])

str_to_int ( ch1 : ch2 : s )
	= (ch1*256+ch2,s) 





str_to_list :: ([Int] -> (b, [Int])) -> [Int] -> ([b], [Int])

str_to_list str_to_fn s 
	= f i s1 
	  where
	  f 0 s = ([],s)
      	  f i s = (obj:objL,s2)
		  where
		  (obj,s1)  = str_to_fn s
		  (objL,s2) = f (i-1) s1
	  (i, s1) = str_to_int s




encode :: (Eq b) => b -> [b] -> Int

encode obj objL 
	= enc 0 objL 
	  where
	  enc i [] = error "Encode" -- ** exn
	  enc i (obj1:objL) 
		| obj == obj1 = i 
		| otherwise   = enc (i+1) objL
    
