-- !!! Testing the Prelude's Enum instances.
{-# LANGUAGE CPP #-}
module Main(main) where

import Control.Exception
import Data.Char
import Data.Ratio

main = do
   -- Enum Int
  putStrLn "Testing Enum Int: "
  testEnumInt
   -- Enum Integer
  putStrLn "Testing Enum Integer: "
  testEnumInteger
   -- Enum Char
  putStrLn "Testing Enum Char: "
  testEnumChar
   -- Enum ()
  putStrLn "Testing Enum (): "
  testEnumUnit
   -- Enum Ordering
  putStrLn "Testing Enum Ordering (derived): "
  testEnumOrdering
   -- Enum Bool
  putStrLn "Testing Enum Bool: "
  testEnumBool
   -- Enum Rational
  putStrLn "Testing Enum Rational: "
  testEnumRational
   -- Enum (Ratio Int)
  putStrLn "Testing Enum (Ratio Int): "
  testEnumRatioInt

{- 
  Here's the properties that's supposed to
  hold for arithmetic sequences over Int:

   - [e1..]    = [e1, (e1+1), (e1+2), ..., maxBound]

   - [e1,e2..] = [e1, (e1+i), (e1+2*i), ... upper]
                 where
		  i = e2 - e1
		  upper
		   | i >  0 = maxBound
		   | i <  0 = minBound
		   | i == 0 = maxBound -- this really shouldn't matter (I feel.)
   - [e1..e3] = [e1, (e1+i), (e1+2*i),..e3]
                where
		 i
		  | e3 >= e1 = 1
		  | e3 <  e1 = (-1)
    
   - [e1,e2..e3] = res
                   where
		    i = e2 - e1
		    
		    res
		     | i >= 0 && e3 <  e1 = []
		     | i <  0 && e3 >= e1 = []  -- (*)
		     | otherwise          = [e1, (e1+i), (e1 + 2*i), .. e3]

   Note:
     (*) - I think this instead should be (i < 0 && e3 > e1), since, as is,

            [x,(x+1) ..x] = [x] 
            [x,(x-1) ..x] = []

           which does not look right, symmetrically speaking.


   The same properties hold for other Prelude types that
   are instances of Enum as well as being Bounded.
   
   For non-Bounded types (e.g., Float and Double), the properties are similar,
   except that the boundary tests become slightly different, i.e., when an
   element becomes greater than (e3 + i/2) (or less than (e3 + i/2) for negative
   i.)

  Q - does [(x::Double)..] have an upper bound? (ditto for Float.)

  OK - on with the regression testing.
-}

#define printTest(x) (do{ putStr ( "    " ++ "x" ++ " = " ) ; print (x) })


testEnumInt :: IO ()
testEnumInt = do
     -- succ
  printTest ((succ (0::Int)))
  printTest ((succ (minBound::Int)))
  mayBomb   (printTest ((succ (maxBound::Int))))

     -- pred
  printTest (pred (1::Int))
  printTest (pred (maxBound::Int))
  mayBomb   (printTest (pred (minBound::Int))) 

     -- toEnum
  printTest ((map (toEnum::Int->Int) [1,minBound,maxBound]))

     -- fromEnum
  printTest ((map fromEnum [(1::Int),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Int)..]))
  printTest ((take 7 [((maxBound::Int)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Int),2..]))
  printTest ((take 7 [(1::Int),7..]))
  printTest ((take 7 [(1::Int),1..]))
  printTest ((take 7 [(1::Int),0..]))
  printTest ((take 7 [(5::Int),2..]))
  let x = (minBound::Int) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Int) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Int) - 5
  printTest ((take 7 [x, (x+1) ..]))

	-- Test overflow conditions
  printTest (([minBound::Int,1..]))
  printTest (([minBound::Int,0..]))
  printTest (([minBound::Int,-1..]))
  printTest (([maxBound::Int,1..]))
  printTest (([maxBound::Int,0..]))
  printTest (([maxBound::Int,-1..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Int) .. 5])))
  printTest ((take 4 ([(1::Int) .. 1])))
  printTest ((take 7 ([(1::Int) .. 0])))
  printTest ((take 7 ([(5::Int) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Int)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Int)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Int),4..1]))
  printTest ((take 7 [(5::Int),3..1]))
  printTest ((take 7 [(5::Int),3..2]))
  printTest ((take 7 [(1::Int),2..1]))
  printTest ((take 7 [(2::Int),1..2]))
  printTest ((take 7 [(2::Int),1..1]))
  printTest ((take 7 [(2::Int),3..1]))

	-- Test overflow conditions
  printTest (([minBound, 1..maxBound::Int]))
  printTest (([minBound, 0..maxBound::Int]))
  printTest (([minBound,-1..maxBound::Int]))
  printTest (([minBound,-1..maxBound-1::Int]))
  printTest (([minBound,-1..maxBound-2::Int]))

  printTest (([maxBound, 1..minBound::Int]))
  printTest (([maxBound, 0..minBound::Int]))
  printTest (([maxBound, 0..minBound+1::Int]))
  printTest (([maxBound, 0..minBound+2::Int]))
  printTest (([maxBound,-1..minBound::Int]))

  let x = (maxBound::Int) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Int) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumChar :: IO ()
testEnumChar = do
     -- succ
  printTest ((succ 'a'))
  printTest ((succ (minBound::Char)))
  mayBomb (printTest ((succ (maxBound::Char))))

     -- pred
  printTest ((pred 'b'))
  printTest (pred (maxBound::Char))
  mayBomb (printTest (pred (minBound::Char)))

     -- toEnum
  printTest ((map (toEnum::Int->Char) [123,ord (minBound::Char), ord(maxBound::Char)]))
  mayBomb (printTest ((toEnum::Int->Char) (minBound::Int)))

     -- fromEnum
  printTest ((map fromEnum ['X',minBound,maxBound]))

     -- [x..] aka enumFrom
  -- printTest ((take 7 ['\NUL' .. ]))
  do{ putStr ( "    " ++ "(take 7 ['\\NUL' .. ])" ++ " = " ) ; print (take 7 ['\NUL' .. ]) }
  -- printTest ((take 7 ['\250' .. ]))
  do{ putStr ( "    " ++ "(take 7 ['\\250' .. ])" ++ " = " ) ; print (take 7 ['\250' .. ]) }
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 ['a','b'..]))
  printTest ((take 7 ['a','e'..]))
  printTest ((take 7 ['a','a'..]))
  printTest ((take 7 ['z','y'..]))
  printTest ((take 7 ['z','v'..]))
  let x = '\1'
  -- printTest ((take 7 ['\1', '\0' ..]))
  do{ putStr ( "    " ++ "(take 7 ['\\1', '\\0' ..])" ++ " = " ) ; print (take 7 ['\1', '\0' ..]) }
  let x = '\5'
  -- printTest ((take 7 ['\5', '\4' ..]))
  do{ putStr ( "    " ++ "(take 7 ['\\5', '\\4' ..])" ++ " = " ) ; print (take 7 ['\5', '\4' ..]) }
  let x = (maxBound::Int) - 5
  -- printTest ((take 7 ['\250', '\251' ..]))
  do{ putStr ( "    " ++ "(take 7 ['\\250', '\\251' ..])" ++ " = " ) ; print (take 7 ['\250', '\251' ..]) }

     -- [x..y] aka enumFromTo
  printTest ((take 7 (['a' .. 'e'])))
  printTest ((take 4 (['a' .. 'a'])))
  printTest ((take 7 (['b' .. 'a'])))
  printTest ((take 7 (['e' .. 'a'])))
  -- printTest ((take 7 (['\250' .. '\255'])))
  do{ putStr ( "    " ++ "(take 7 (['\\250' .. '\\255']))" ++ " = " ) ; print (take 7 (['\250' .. '\255'])) }
  -- printTest ((take 7 (['\5' .. '\0'])))
  do{ putStr ( "    " ++ "(take 7 (['\\5' .. '\\0']))" ++ " = " ) ; print (take 7 (['\5' .. '\0'])) }

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 ['f','e' .. 'b']))
  printTest ((take 7 ['g','e' .. 'b']))
  printTest ((take 7 ['g','d' .. 'c']))
  printTest ((take 7 ['b','c' .. 'b']))
  printTest ((take 7 ['c','b' .. 'c']))
  printTest ((take 7 ['c','b' .. 'b']))
  printTest ((take 7 ['c','d' .. 'b']))
  -- printTest ((take 7 ['\251', '\252' .. maxBound]))
  do{ putStr ( "    " ++ "(take 7 ['\\251', '\\252' .. maxBound])" ++ " = " ) ; print (take 7 ['\251', '\252' .. maxBound]) }
  -- printTest ((take 7 ['\5', '\4' .. minBound]))
  do{ putStr ( "    " ++ "(take 7 ['\\5', '\\4' .. minBound])" ++ " = " ) ; print (take 7 ['\5', '\4' .. minBound]) }


testEnumUnit :: IO ()
testEnumUnit = do
   -- succ:
  mayBomb (printTest ((succ ())))
  mayBomb (printTest ((succ (minBound::()))))
  mayBomb (printTest ((succ (maxBound::()))))
       
   -- pred:
  mayBomb (printTest ((pred ())))
  mayBomb (printTest ((pred (minBound::()))))
  mayBomb (printTest ((pred (maxBound::()))))

   -- toEnum:
  printTest ((toEnum 0)::())
  mayBomb   (printTest ((toEnum 1)::()))

   -- fromEnum:
  printTest ((fromEnum ()))

   -- enumFrom:
  printTest ((take 7 [()..]))

   -- enumFromThen:
  printTest ((take 7 [(),()..]))

   -- enumFromTo
  printTest ((take 7 [()..()]))

   -- enumFromThenTo
  printTest ((take 7 [(),()..()]))

testEnumOrdering :: IO ()
testEnumOrdering = do
   -- succ:
  printTest ((succ LT))
  printTest ((succ (minBound::Ordering)))
  mayBomb (printTest ((succ (maxBound::Ordering))))
       
   -- pred:
  printTest ((pred GT))
  printTest ((pred (maxBound::Ordering)))
  mayBomb (printTest ((pred (minBound::Ordering))))

   -- toEnum:
  printTest ((toEnum 0)::Ordering)
  mayBomb   (printTest ((toEnum 5)::Ordering))

   -- fromEnum:
  printTest ((fromEnum LT))
  printTest ((fromEnum EQ))
  printTest ((fromEnum GT))

   -- enumFrom:
  printTest (([LT ..]))
  printTest (([EQ ..]))
  printTest (([GT ..]))

   -- enumFromThen:
  printTest (([LT,EQ ..]))
  printTest (([EQ,GT ..]))
  printTest (([EQ,LT ..]))
  printTest (([LT,GT ..]))
  printTest (([GT,LT ..]))
  printTest (take 7 (([GT,GT ..])))
  printTest (take 7 (([LT,LT ..])))

   -- enumFromTo
  printTest (([LT .. GT]))
  printTest (([LT .. EQ]))
  printTest (([LT .. LT]))
  printTest (([GT .. LT]))
  printTest (([GT .. EQ]))
  printTest (([GT .. GT]))

   -- enumFromThenTo
  printTest (([LT,EQ .. GT]))
  printTest (([GT,EQ .. LT]))
  printTest (([GT,EQ .. EQ]))
  printTest (([GT,EQ .. GT]))
  printTest (([GT,EQ .. LT]))
  printTest (([LT,EQ .. LT]))
  printTest (([LT,EQ .. GT]))
  printTest (take 7 (([LT,LT .. GT])))
  printTest (take 7 (([GT,GT .. LT])))

testEnumBool :: IO ()
testEnumBool = do
   -- succ:
  printTest ((succ False))
  printTest ((succ (minBound::Bool)))
  mayBomb (printTest ((succ (maxBound::Bool))))
       
   -- pred:
  printTest ((pred True))
  printTest ((pred (maxBound::Bool)))
  mayBomb (printTest ((pred (minBound::Bool))))

   -- toEnum:
  printTest ((toEnum 0)::Bool)
  mayBomb   (printTest ((toEnum 5)::Bool))

   -- fromEnum:
  printTest ((fromEnum False))
  printTest ((fromEnum True))

   -- enumFrom:
  printTest (([False ..]))
  printTest (([True ..]))

   -- enumFromThen:
  printTest (([False,True ..]))
  printTest (([True,False ..]))
  printTest ((take 7 ([False,False ..])))
  printTest ((take 7 ([True,True ..])))

   -- enumFromTo
  printTest (([False .. True]))
  printTest (([True .. False]))

   -- enumFromThenTo
  printTest (take 7 ([False,False .. False]))
  printTest (take 7 ([False,False .. True]))
  printTest (take 7 ([False,True .. False]))
  printTest (take 7 ([False,True .. True]))
  printTest (take 7 ([True,False .. False]))
  printTest (take 7 ([True,False .. True]))
  printTest (take 7 ([True,True .. False]))
  printTest (take 7 ([True,True .. True]))


testEnumInteger :: IO ()
testEnumInteger = do
     -- succ
  printTest ((succ (0::Integer)))
  printTest ((succ ((-1)::Integer)))

     -- pred
  printTest (pred (1::Integer))
  printTest (pred (0::Integer))

     -- toEnum
  printTest ((map (toEnum::Int->Integer) [1,minBound,maxBound]))

     -- fromEnum
  printTest ((map fromEnum [(1::Integer),42,45]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Integer)..]))
  printTest ((take 7 [(-5::Integer)..]))
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Integer),2..]))
  printTest ((take 7 [(1::Integer),7..]))
  printTest ((take 7 [(1::Integer),1..]))
  printTest ((take 7 [(1::Integer),0..]))
  printTest ((take 7 [(5::Integer),2..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Integer) .. 5])))
  printTest ((take 4 ([(1::Integer) .. 1])))
  printTest ((take 7 ([(1::Integer) .. 0])))
  printTest ((take 7 ([(5::Integer) .. 0])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Integer),4..1]))
  printTest ((take 7 [(5::Integer),3..1]))
  printTest ((take 7 [(5::Integer),3..2]))
  printTest ((take 7 [(1::Integer),2..1]))
  printTest ((take 7 [(2::Integer),1..2]))
  printTest ((take 7 [(2::Integer),1..1]))
  printTest ((take 7 [(2::Integer),3..1]))

testEnumRational :: IO ()
testEnumRational = do
     -- succ
  printTest ((succ (0::Rational)))
  printTest ((succ ((-1)::Rational)))

     -- pred
  printTest (pred (1::Rational))
  printTest (pred (0::Rational))

     -- toEnum
  printTest ((map (toEnum::Int->Rational) [1,minBound,maxBound]))

     -- fromEnum
  printTest ((map fromEnum [(1::Rational),42,45]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Rational)..]))
  printTest ((take 7 [(-5::Rational)..]))
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Rational),2..]))
  printTest ((take 7 [(1::Rational),7..]))
  printTest ((take 7 [(1::Rational),1..]))
  printTest ((take 7 [(1::Rational),0..]))
  printTest ((take 7 [(5::Rational),2..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Rational) .. 5])))
  printTest ((take 4 ([(1::Rational) .. 1])))
  printTest ((take 7 ([(1::Rational) .. 0])))
  printTest ((take 7 ([(5::Rational) .. 0])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Rational),4..1]))
  printTest ((take 7 [(5::Rational),3..1]))
  printTest ((take 7 [(5::Rational),3..2]))
  printTest ((take 7 [(1::Rational),2..1]))
  printTest ((take 7 [(2::Rational),1..2]))
  printTest ((take 7 [(2::Rational),1..1]))
  printTest ((take 7 [(2::Rational),3..1]))

testEnumRatioInt :: IO ()
testEnumRatioInt = do
     -- succ
  printTest ((succ (0::Ratio Int)))
  printTest ((succ ((-1)::Ratio Int)))

     -- pred
  printTest (pred (1::Ratio Int))
  printTest (pred (0::Ratio Int))

     -- toEnum
  printTest ((map (toEnum::Int->Ratio Int) [1,minBound,maxBound]))

     -- fromEnum
  printTest ((map fromEnum [(1::Ratio Int),42,45]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Ratio Int)..]))
  printTest ((take 7 [(-5::Ratio Int)..]))
  printTest ((take 7 [((toEnum ((maxBound::Int)-5))::Ratio Int)..]))
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Ratio Int),2..]))
  printTest ((take 7 [(1::Ratio Int),7..]))
  printTest ((take 7 [(1::Ratio Int),1..]))
  printTest ((take 7 [(1::Ratio Int),0..]))
  printTest ((take 7 [(5::Ratio Int),2..]))
  let x = (toEnum ((minBound::Int) + 1))::Ratio Int
  printTest ((take 7 [x, x-1 ..]))
  let x = (toEnum ((minBound::Int) + 5))::Ratio Int
  printTest ((take 7 [x, x-1 ..]))
  let x = (toEnum ((maxBound::Int) - 5))::Ratio Int
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Ratio Int) .. 5])))
  printTest ((take 4 ([(1::Ratio Int) .. 1])))
  printTest ((take 7 ([(1::Ratio Int) .. 0])))
  printTest ((take 7 ([(5::Ratio Int) .. 0])))
  let x = (toEnum (maxBound - (5::Int))) :: Ratio Int
  let y = (toEnum (maxBound::Int)) :: Ratio Int
  printTest ((take 7 ([x..y])))
  let x = (toEnum (minBound + (5::Int))) :: Ratio Int
  let y = (toEnum (minBound::Int)) :: Ratio Int
  printTest ((take 7 ([x..y])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Ratio Int),4..1]))
  printTest ((take 7 [(5::Ratio Int),3..1]))
  printTest ((take 7 [(5::Ratio Int),3..2]))
  printTest ((take 7 [(1::Ratio Int),2..1]))
  printTest ((take 7 [(2::Ratio Int),1..2]))
  printTest ((take 7 [(2::Ratio Int),1..1]))
  printTest ((take 7 [(2::Ratio Int),3..1]))

  let x = (toEnum ((maxBound::Int) - 4)) :: Ratio Int
  let y = (toEnum (maxBound::Int)) :: Ratio Int
  printTest ((take 7 [x,(x+1)..y]))
  let x = (toEnum ((minBound::Int) + 5)) :: Ratio Int
  let y = (toEnum (minBound::Int)) :: Ratio Int
  printTest ((take 7 [x,(x-1)..y]))

--
--
--  Utils
--
--


mayBomb x = catch x (\(ErrorCall e) -> putStrLn ("error " ++ show e))
   `catch` (\e -> putStrLn ("Fail: " ++ show (e :: SomeException)))

test :: Show a => String -> String -> a -> IO ()
test test_nm expected val = do
   putStr test_nm
   if expected == got then
      putStrLn ": SUCCEEDED"
    else do
      putStr   ": FAILED"
      putStrLn ("( expected: " ++ show expected ++ " , got: " ++ show got ++ " )")
  where
   got = show val
