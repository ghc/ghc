module Utilities (toBinary, fl) where

import Stream
import Data.Ratio

-- Convert from an Integer to its signed-digit representation
toBinary :: Integer -> Stream
toBinary 0 = [0]
toBinary x = toBinary t ++ [x `mod` 2]
	     where t = x `div` 2
	     


fl :: Stream -> Stream
fl (x:xs) = (f x):xs
	  where f 0 = 1
	  	f 1 = 0
