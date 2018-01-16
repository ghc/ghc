{-
 - BinConv.hs
 -
 - Paul Sanders, SRD. 1992
 -
 - This module contains routines for converting numbers to and from a
 - number of binary digits. 
 -
 -}

module BinConv (codes_to_ascii, ascii_to_codes, dec_to_binx) where

zeroes = '0' : zeroes

-- dec_to_binx converts a decimal to a fixed number of binary digits
-- dec_to_binx #binary-digits decimal-number = binary-string

dec_to_binx :: Int -> Int -> String
dec_to_binx x y
     = take (x - length bin_string) zeroes ++ bin_string
       where
       bin_string = dec_to_bin y

dec_to_bin = reverse . dec_to_bin'

dec_to_bin' 0 = []
dec_to_bin' x
      = (if (x `rem` 2) == 1 
         then '1' 
         else '0') : dec_to_bin' (x `div` 2)

codes_to_ascii :: [Int] -> [Int]
codes_to_ascii [] = []
codes_to_ascii (x:y:ns)
	= x_div : ((x_rem * 16) + y_div) : y_rem : codes_to_ascii ns
          where
          (x_div, x_rem) = divRem x 16
          (y_div, y_rem) = divRem y 256
codes_to_ascii [n]
	= [x_div , x_rem]
          where
          (x_div, x_rem) = divRem n 16

ascii_to_codes [] = []
ascii_to_codes (x:y:z:ns)
	= (x * 16) + y_div : (y_rem * 256) + z : ascii_to_codes ns
	  where
	  (y_div, y_rem) = divRem y 16
ascii_to_codes [x,y]
	= [(x * 16) + y_rem]
	  where
	  (y_div, y_rem) = divRem y 16

divRem x y = (x `div` y, x `rem` y) -- missing from PreludeCore ?
