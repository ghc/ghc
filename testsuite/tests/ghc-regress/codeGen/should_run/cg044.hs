{-# OPTIONS -cpp #-}
-- !!! Testing IEEE Float and Double extremity predicates.
module Main(main) where

import Data.Char
import Control.Monad.ST
import Data.Word
import Data.Array.ST

#include "config.h"

reverse_if_bigendian :: [a] -> [a]
#ifdef WORDS_BIGENDIAN
reverse_if_bigendian = reverse
#else
reverse_if_bigendian = id
#endif


main :: IO ()
main = do
 sequence_ (map putStrLn double_tests)
 sequence_ (map putStrLn float_tests)
  where
   double_tests = run_tests double_numbers
   float_tests  = run_tests float_numbers  

   run_tests nums =
    map ($ nums)
        [ denorm
        , pos_inf
        , neg_inf
        , nan
        , neg_zero
        , pos_zero
        ]

-------------
double_numbers :: [Double]
double_numbers =
      [ 0
      , encodeFloat 0 0     -- 0 using encodeFloat method
      , mkDouble (reverse_if_bigendian [0,0,0,0,0,0, 0xf0, 0x7f])  -- +inf
      , encodeFloat 1 2047  -- +Inf 
      , encodeFloat 1 2048
      , encodeFloat 1  2047		  -- signalling NaN
      , encodeFloat 0xf000000000000 2047  -- quiet NaN
      , 0/(0::Double)
        -- misc
      , 1.82173691287639817263897126389712638972163e-300
      , 1.82173691287639817263897126389712638972163e+300
      , 4.9406564558412465e-324  -- smallest possible denorm number 
				 -- (as reported by enquire running
				 --  on a i686-pc-linux.)
      , 2.2250738585072014e-308
      , 0.11
      , 0.100
      , -3.4
        -- smallest 
      , let (l, _) = floatRange x
            x = encodeFloat 1 (l-1)
	in x
        -- largest
      , let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in x
      ]

float_numbers :: [Float]
float_numbers =
      [ 0
      , encodeFloat 0 0     -- 0 using encodeFloat method
      , encodeFloat 1 255  -- +Inf 
      , encodeFloat 1 256
      , encodeFloat 11 255	  -- signalling NaN
      , encodeFloat 0xf00000 255  -- quiet NaN
      , 0/(0::Float)
        -- misc
      , 1.82173691287639817263897126389712638972163e-300
      , 1.82173691287639817263897126389712638972163e+300
      , 1.40129846e-45
      , 1.17549435e-38
      , 2.98023259e-08
      , 0.11
      , 0.100
      , -3.4
        -- smallest 
      , let (l, _) = floatRange x
            x = encodeFloat 1 (l-1)
	in x
        -- largest
      , let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in x
      ]

-------------

denorm :: RealFloat a => [a] -> String
denorm numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Denormalised numbers: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isDenormalized) "isDenormalised"

pos_inf :: RealFloat a => [a] -> String
pos_inf numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Positive Infinity: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isInfinite) "isInfinite"

neg_inf :: RealFloat a => [a] -> String
neg_inf numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Negative Infinity: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (\ x -> isInfinite x && x < 0) "isNegInfinite"

nan :: RealFloat a => [a] -> String
nan numbers =
  unlines
     ( ""
     : "*********************************"
     : ("NaN: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isNaN) "isNaN"

pos_zero :: RealFloat a => [a] -> String
pos_zero numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Positive zero: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (==0) "isPosZero"

neg_zero :: RealFloat a => [a] -> String
neg_zero numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Negative zero: " ++ doubleOrFloat numbers)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isNegativeZero) "isNegativeZero"

-- what a hack.
doubleOrFloat :: RealFloat a => [a] -> String
doubleOrFloat ls
 | (floatDigits atType) == (floatDigits (0::Double)) = "Double"
 | (floatDigits atType) == (floatDigits (0::Float))  = "Float"
 | otherwise = "unknown RealFloat type"
 where
   atType = undefined `asTypeOf` (head ls)

-- make a double from a list of 8 bytes
-- (caller deals with byte ordering.)
mkDouble :: [Word8] -> Double
mkDouble ls = 
 runST (( do
   arr <- newArray_ (0,7)
   sequence (zipWith (writeArray arr) [(0::Int)..] (take 8 ls))
   arr' <- castSTUArray arr
   readArray arr' 0
 ) :: ST s Double )

showAndPerform :: (Show a, Show b)
	       => (a -> b)
	       -> String
	       -> a
	       -> String
showAndPerform fun name_fun val =
  name_fun ++ ' ':show val ++ " = " ++ show (fun val)


