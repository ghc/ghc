--!!! Testing IEEE Float and Double extremity predicates.
module Main(main) where

printLn :: Show a => a -> IO ()
printLn v = putStrLn (show v)

main = do
 sequence (map putStrLn double_tests)
 sequence (map putStrLn float_tests)
  where
    -- dummy arg used to resolve what
    -- instance of RealFloat we're interested in.
   double_tests = run_tests double_numbers
   float_tests  = run_tests float_numbers  

   run_tests nums =
     let atTy = (undefined `asTypeOf` (head nums))  in
     [ denorm   atTy nums
     , pos_inf  atTy nums
     , neg_inf  atTy nums
     , nan      atTy nums
     , neg_zero atTy nums
     , pos_zero atTy nums
     ]

denorm :: RealFloat a => a -> [a] -> String
denorm atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Denormalised numbers: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isDenormalized) "isDenormalised"

pos_inf :: RealFloat a => a -> [a] -> String
pos_inf atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Positive Infinity: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isInfinite) "isInfinite"

neg_inf :: RealFloat a => a -> [a] -> String
neg_inf atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Negative Infinity: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (\ x -> isInfinite x && x < 0) "isNegInfinite"

nan :: RealFloat a => a -> [a] -> String
nan atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("NaN: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isNaN) "isNaN"

pos_zero :: RealFloat a => a -> [a] -> String
pos_zero atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Positive zero: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (==0) "isPosZero"

neg_zero :: RealFloat a => a -> [a] -> String
neg_zero atType numbers =
  unlines
     ( ""
     : "*********************************"
     : ("Negative zero: " ++ doubleOrFloat atType)
     : ""
     : map showPerform numbers)
 where
   showPerform = showAndPerform (isNegativeZero) "isNegativeZero"

-- what a hack.
doubleOrFloat :: RealFloat a => a -> String
doubleOrFloat atType
 | (floatDigits atType) == (floatDigits (0::Double)) = "Double"
 | (floatDigits atType) == (floatDigits (0::Float))  = "Float"
 | otherwise = "unknown RealFloat type"

double_numbers :: [Double]
double_numbers =
      [ 0
      , encodeFloat 0 0     -- 0 using encodeFloat method
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

showAndPerform :: (Show a, Show b)
	       => (a -> b)
	       -> String
	       -> a
	       -> String
showAndPerform fun name_fun val =
  name_fun ++ ' ':show val ++ " = " ++ show (fun val)

