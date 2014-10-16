-- !!! Testing the Word Enum instances.
{-# LANGUAGE CPP #-}
module Main(main) where

import Control.Exception
import Data.Word
import Data.Int

main = do
  putStrLn "Testing Enum Word8:"
  testEnumWord8
  putStrLn "Testing Enum Word16:"
  testEnumWord16
  putStrLn "Testing Enum Word32:"
  testEnumWord32
  putStrLn "Testing Enum Word64:"
  testEnumWord64


#define printTest(x) (do{ putStr ( "    " ++ "x" ++ " = " ) ; print (x) })

testEnumWord8 :: IO ()
testEnumWord8 = do
     -- succ
  printTest ((succ (0::Word8)))
  printTest ((succ (minBound::Word8)))
  mayBomb   (printTest ((succ (maxBound::Word8))))

     -- pred
  printTest (pred (1::Word8))
  printTest (pred (maxBound::Word8))
  mayBomb   (printTest (pred (minBound::Word8))) 

     -- toEnum
  printTest ((map (toEnum::Int->Word8) [1, fromIntegral (minBound::Word8)::Int, fromIntegral (maxBound::Word8)::Int]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Word8))

     -- fromEnum
  printTest ((map fromEnum [(1::Word8),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Word8)..]))
  printTest ((take 7 [((maxBound::Word8)-5)..])) -- just in case it doesn't catch the upper bound..

     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Word8),2..]))
  printTest ((take 7 [(1::Word8),7..]))
  printTest ((take 7 [(1::Word8),1..]))
  printTest ((take 7 [(1::Word8),0..]))
  printTest ((take 7 [(5::Word8),2..]))
  let x = (minBound::Word8) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Word8) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Word8) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Word8) .. 5])))
  printTest ((take 4 ([(1::Word8) .. 1])))
  printTest ((take 7 ([(1::Word8) .. 0])))
  printTest ((take 7 ([(5::Word8) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Word8)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Word8)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Word8),4..1]))
  printTest ((take 7 [(5::Word8),3..1]))
  printTest ((take 7 [(5::Word8),3..2]))
  printTest ((take 7 [(1::Word8),2..1]))
  printTest ((take 7 [(2::Word8),1..2]))
  printTest ((take 7 [(2::Word8),1..1]))
  printTest ((take 7 [(2::Word8),3..1]))

  let x = (maxBound::Word8) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Word8) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumWord16 :: IO ()
testEnumWord16 = do
     -- succ
  printTest ((succ (0::Word16)))
  printTest ((succ (minBound::Word16)))
  mayBomb   (printTest ((succ (maxBound::Word16))))

     -- pred
  printTest (pred (1::Word16))
  printTest (pred (maxBound::Word16))
  mayBomb   (printTest (pred (minBound::Word16))) 

     -- toEnum
  printTest ((map (toEnum::Int->Word16) [1, fromIntegral (minBound::Word16)::Int, fromIntegral (maxBound::Word16)::Int]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Word16))


     -- fromEnum
  printTest ((map fromEnum [(1::Word16),minBound,maxBound]))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Word16)..]))
  printTest ((take 7 [((maxBound::Word16)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Word16),2..]))
  printTest ((take 7 [(1::Word16),7..]))
  printTest ((take 7 [(1::Word16),1..]))
  printTest ((take 7 [(1::Word16),0..]))
  printTest ((take 7 [(5::Word16),2..]))
  let x = (minBound::Word16) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Word16) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Word16) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Word16) .. 5])))
  printTest ((take 4 ([(1::Word16) .. 1])))
  printTest ((take 7 ([(1::Word16) .. 0])))
  printTest ((take 7 ([(5::Word16) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Word16)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Word16)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Word16),4..1]))
  printTest ((take 7 [(5::Word16),3..1]))
  printTest ((take 7 [(5::Word16),3..2]))
  printTest ((take 7 [(1::Word16),2..1]))
  printTest ((take 7 [(2::Word16),1..2]))
  printTest ((take 7 [(2::Word16),1..1]))
  printTest ((take 7 [(2::Word16),3..1]))

  let x = (maxBound::Word16) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Word16) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumWord32 :: IO ()
testEnumWord32 = do
     -- succ
  printTest ((succ (0::Word32)))
  printTest ((succ (minBound::Word32)))
  mayBomb   (printTest ((succ (maxBound::Word32))))

     -- pred
  printTest (pred (1::Word32))
  printTest (pred (maxBound::Word32))
  mayBomb   (printTest (pred (minBound::Word32))) 

     -- toEnum
  printTest ((map (toEnum::Int->Word32) [1, fromIntegral (minBound::Word32)::Int, fromIntegral (maxBound::Int32)::Int]))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Word32))

     -- fromEnum
  printTest ((map fromEnum [(1::Word32),minBound,fromIntegral (maxBound::Int)]))
  mayBomb   (printTest (fromEnum (maxBound::Word32)))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Word32)..]))
  printTest ((take 7 [((maxBound::Word32)-5)..])) -- just in case it doesn't catch the upper bound..
  
     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Word32),2..]))
  printTest ((take 7 [(1::Word32),7..]))
  printTest ((take 7 [(1::Word32),1..]))
  printTest ((take 7 [(1::Word32),0..]))
  printTest ((take 7 [(5::Word32),2..]))
  let x = (minBound::Word32) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Word32) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Word32) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Word32) .. 5])))
  printTest ((take 4 ([(1::Word32) .. 1])))
  printTest ((take 7 ([(1::Word32) .. 0])))
  printTest ((take 7 ([(5::Word32) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Word32)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Word32)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Word32),4..1]))
  printTest ((take 7 [(5::Word32),3..1]))
  printTest ((take 7 [(5::Word32),3..2]))
  printTest ((take 7 [(1::Word32),2..1]))
  printTest ((take 7 [(2::Word32),1..2]))
  printTest ((take 7 [(2::Word32),1..1]))
  printTest ((take 7 [(2::Word32),3..1]))

  let x = (maxBound::Word32) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Word32) + 5
  printTest ((take 7 [x,(x-1)..minBound]))

testEnumWord64 :: IO ()
testEnumWord64 = do
     -- succ
  printTest ((succ (0::Word64)))
  printTest ((succ (minBound::Word64)))
  mayBomb   (printTest ((succ (maxBound::Word64))))

     -- pred
  printTest (pred (1::Word64))
  printTest (pred (maxBound::Word64))
  mayBomb   (printTest (pred (minBound::Word64))) 

     -- toEnum
  mayBomb   (printTest ((map (toEnum::Int->Word64) [1, fromIntegral (minBound::Word64)::Int, maxBound::Int])))
  mayBomb   (printTest ((toEnum (maxBound::Int))::Word64))

     -- fromEnum
  printTest ((map fromEnum [(1::Word64),minBound,fromIntegral (maxBound::Int)]))
  mayBomb   (printTest (fromEnum (maxBound::Word64)))

     -- [x..] aka enumFrom
  printTest ((take 7 [(1::Word64)..]))
  printTest ((take 7 [((maxBound::Word64)-5)..])) -- just in case it doesn't catch the upper bound..

     -- [x,y..] aka enumFromThen
  printTest ((take 7 [(1::Word64),2..]))
  printTest ((take 7 [(1::Word64),7..]))
  printTest ((take 7 [(1::Word64),1..]))
  printTest ((take 7 [(1::Word64),0..]))
  printTest ((take 7 [(5::Word64),2..]))
  let x = (minBound::Word64) + 1
  printTest ((take 7 [x, x-1 ..]))
  let x = (minBound::Word64) + 5
  printTest ((take 7 [x, x-1 ..]))
  let x = (maxBound::Word64) - 5
  printTest ((take 7 [x, (x+1) ..]))

     -- [x..y] aka enumFromTo
  printTest ((take 7 ([(1::Word64) .. 5])))
  printTest ((take 4 ([(1::Word64) .. 1])))
  printTest ((take 7 ([(1::Word64) .. 0])))
  printTest ((take 7 ([(5::Word64) .. 0])))
  printTest ((take 7 ([(maxBound-(5::Word64)) .. maxBound])))
  printTest ((take 7 ([(minBound+(5::Word64)) .. minBound])))

     -- [x,y..z] aka enumFromThenTo
  printTest ((take 7 [(5::Word64),4..1]))
  printTest ((take 7 [(5::Word64),3..1]))
  printTest ((take 7 [(5::Word64),3..2]))
  printTest ((take 7 [(1::Word64),2..1]))
  printTest ((take 7 [(2::Word64),1..2]))
  printTest ((take 7 [(2::Word64),1..1]))
  printTest ((take 7 [(2::Word64),3..1]))

  let x = (maxBound::Word64) - 4
  printTest ((take 7 [x,(x+1)..maxBound]))
  let x = (minBound::Word64) + 5
  printTest ((take 7 [x,(x-1)..minBound]))


--
--
--  Utils
--
--


mayBomb x = catch x (\(ErrorCall e) -> putStrLn ("error " ++ show e))
  `catch` (\e -> putStrLn ("Fail: " ++ show (e :: SomeException)))
