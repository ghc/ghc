-- !!! Testing the Int Enum instances.
{-# OPTIONS_GHC -F -pgmF ./enum_processor.py #-}
-- The processor is a non-CPP-based equivalent of
-- #define printTest(x) (do{ putStr ( "    " ++ "x" ++ " = " ) ; print (x) })
-- which is not portable to clang

module Main(main) where

import Control.Exception
import Data.Int

main = do
  putStrLn "Testing Enum Int8:"
  testEnumInt8
  putStrLn "Testing Enum Int16:"
  testEnumInt16
  putStrLn "Testing Enum Int32:"
  testEnumInt32
  putStrLn "Testing Enum Int64:"
  testEnumInt64


testEnumInt8 :: IO ()
testEnumInt8 = do
     -- succ
  printTest ((succ (0::Int8)))
  printTest ((succ (minBound::Int8)))
  mayBomb   (printTest ((succ (maxBound::Int8))))

     -- pred
  printTest (pred (1::Int8))
  printTest (pred (maxBound::Int8))
  mayBomb   (printTest (pred (minBound::Int8))) 

     -- toEnum
  printTest ((map (toEnum::Int->Int8) [1, fromIntegral (minBound::Int8), fromIntegral (maxBound::Int8)]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Int8))

     -- fromEnum
  printTest ((map fromEnum [(1::Int8),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Int8)..]))
  printTest ((take 7 [((maxBound::Int8)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Int8),2..]))
  printTest ((take 7 [(1::Int8),7..]))
  printTest ((take 7 [(1::Int8),1..]))
  printTest ((take 7 [(1::Int8),0..]))
  printTest ((take 7 [(5::Int8),2..]))
  let x = (minBound::Int8) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Int8) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Int8) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Int8) .. 5])))
  printTest ((take 4 ([(1::Int8) .. 1])))
  printTest ((take 7 ([(1::Int8) .. 0])))
  printTest ((take 7 ([(5::Int8) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Int8)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Int8)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Int8),4..1]))
  printTest ((take 7 [(5::Int8),3..1]))
  printTest ((take 7 [(5::Int8),3..2]))
  printTest ((take 7 [(1::Int8),2..1]))
  printTest ((take 7 [(2::Int8),1..2]))
  printTest ((take 7 [(2::Int8),1..1]))
  printTest ((take 7 [(2::Int8),3..1]))

  let x = (maxBound::Int8) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Int8) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumInt16 :: IO ()
testEnumInt16 = do
     -- succ
  printTest ((succ (0::Int16)))
  printTest ((succ (minBound::Int16)))
  mayBomb   (printTest ((succ (maxBound::Int16))))

     -- pred
  printTest (pred (1::Int16))
  printTest (pred (maxBound::Int16))
  mayBomb   (printTest (pred (minBound::Int16))) 

     -- toEnum
  printTest ((map (toEnum::Int->Int16) [1, fromIntegral (minBound::Int16), fromIntegral (maxBound::Int16)]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Int16))


     -- fromEnum
  printTest ((map fromEnum [(1::Int16),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Int16)..]))
  printTest ((take 7 [((maxBound::Int16)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Int16),2..]))
  printTest ((take 7 [(1::Int16),7..]))
  printTest ((take 7 [(1::Int16),1..]))
  printTest ((take 7 [(1::Int16),0..]))
  printTest ((take 7 [(5::Int16),2..]))
  let x = (minBound::Int16) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Int16) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Int16) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Int16) .. 5])))
  printTest ((take 4 ([(1::Int16) .. 1])))
  printTest ((take 7 ([(1::Int16) .. 0])))
  printTest ((take 7 ([(5::Int16) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Int16)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Int16)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Int16),4..1]))
  printTest ((take 7 [(5::Int16),3..1]))
  printTest ((take 7 [(5::Int16),3..2]))
  printTest ((take 7 [(1::Int16),2..1]))
  printTest ((take 7 [(2::Int16),1..2]))
  printTest ((take 7 [(2::Int16),1..1]))
  printTest ((take 7 [(2::Int16),3..1]))

  let x = (maxBound::Int16) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Int16) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumInt32 :: IO ()
testEnumInt32 = do
     -- succ
  printTest ((succ (0::Int32)))
  printTest ((succ (minBound::Int32)))
  mayBomb   (printTest ((succ (maxBound::Int32))))

     -- pred
  printTest (pred (1::Int32))
  printTest (pred (maxBound::Int32))
  mayBomb   (printTest (pred (minBound::Int32))) 

     -- toEnum
  printTest ((map (toEnum::Int->Int32) [1, fromIntegral (minBound::Int32), fromIntegral (maxBound::Int32)]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Int32))

     -- fromEnum
  printTest ((map fromEnum [(1::Int32),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Int32)..]))
  printTest ((take 7 [((maxBound::Int32)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Int32),2..]))
  printTest ((take 7 [(1::Int32),7..]))
  printTest ((take 7 [(1::Int32),1..]))
  printTest ((take 7 [(1::Int32),0..]))
  printTest ((take 7 [(5::Int32),2..]))
  let x = (minBound::Int32) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Int32) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Int32) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Int32) .. 5])))
  printTest ((take 4 ([(1::Int32) .. 1])))
  printTest ((take 7 ([(1::Int32) .. 0])))
  printTest ((take 7 ([(5::Int32) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Int32)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Int32)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Int32),4..1]))
  printTest ((take 7 [(5::Int32),3..1]))
  printTest ((take 7 [(5::Int32),3..2]))
  printTest ((take 7 [(1::Int32),2..1]))
  printTest ((take 7 [(2::Int32),1..2]))
  printTest ((take 7 [(2::Int32),1..1]))
  printTest ((take 7 [(2::Int32),3..1]))

  let x = (maxBound::Int32) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Int32) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumInt64 :: IO ()
testEnumInt64 = do
     -- succ
  printTest ((succ (0::Int64)))
  printTest ((succ (minBound::Int64)))
  mayBomb   (printTest ((succ (maxBound::Int64))))

     -- pred
  printTest (pred (1::Int64))
  printTest (pred (maxBound::Int64))
  mayBomb   (printTest (pred (minBound::Int64))) 

     -- toEnum
  mayBomb   (printTest ((map (toEnum::Int->Int64) [1, fromIntegral (minBound::Int64), fromIntegral (maxBound::Int64)])))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Int64))

     -- fromEnum
  printTest ((map fromEnum [(1::Int64),fromIntegral (minBound::Int) ,fromIntegral (maxBound::Int)]))
  mayBomb   (printTest (fromEnum (maxBound::Int64)))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Int64)..]))
  printTest ((take 7 [((maxBound::Int64)-5)..])) -- just in case it doesn't catch the upper bound..

     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Int64),2..]))
  printTest ((take 7 [(1::Int64),7..]))
  printTest ((take 7 [(1::Int64),1..]))
  printTest ((take 7 [(1::Int64),0..]))
  printTest ((take 7 [(5::Int64),2..]))
  let x = (minBound::Int64) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Int64) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Int64) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Int64) .. 5])))
  printTest ((take 4 ([(1::Int64) .. 1])))
  printTest ((take 7 ([(1::Int64) .. 0])))
  printTest ((take 7 ([(5::Int64) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Int64)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Int64)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Int64),4..1]))
  printTest ((take 7 [(5::Int64),3..1]))
  printTest ((take 7 [(5::Int64),3..2]))
  printTest ((take 7 [(1::Int64),2..1]))
  printTest ((take 7 [(2::Int64),1..2]))
  printTest ((take 7 [(2::Int64),1..1]))
  printTest ((take 7 [(2::Int64),3..1]))

  let x = (maxBound::Int64) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Int64) + 5
  printTest ((take 7 [x,(x-1)..minBound]))


--
--
--  Utils
--
--


mayBomb x = catch x (\(ErrorCall e) -> putStrLn ("error " ++ show e))
  `catch` (\e -> putStrLn ("Fail: " ++ show (e :: SomeException)))
