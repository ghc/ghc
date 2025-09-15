-- !!! Testing Int and Word and specifically integer division by constants
module Main(main) where

import Data.Int
import Data.Word
import Control.Monad (when, void)
import Data.Bits (Bits, isSigned)

main :: IO ()
main = test

test :: IO ()
test = do
   testIntlike "Int"    (0::Int  )
   testIntlike "Int8"   (0::Int8 )
   testIntlike "Int16"  (0::Int16)
   testIntlike "Int32"  (0::Int32)
   testIntlike "Int64"  (0::Int64)

   testIntlike "Word"   (0::Word  )
   testIntlike "Word8"  (0::Word8 )
   testIntlike "Word16" (0::Word16)
   testIntlike "Word32" (0::Word32)
   testIntlike "Word64" (0::Word64)

testIntlike :: (Bounded a, Integral a, Show a, Bits a) => String -> a -> IO ()
testIntlike name zero = do
  putStrLn $ "--------------------------------"
  putStrLn $ "--Testing " ++ name
  putStrLn $ "--------------------------------"
  -- 1
  putStrLn "divide by 1"
  testWith (1 `asTypeOf` zero)

  -- Powers of 2
  putStrLn "divide by 2"
  testWith (2 `asTypeOf` zero)
  putStrLn "divide by 4"
  testWith (4 `asTypeOf` zero)
  -- Positive constants
  putStrLn "divide by 3"
  testWith (3 `asTypeOf` zero)
  putStrLn "divide by 5"
  testWith (5 `asTypeOf` zero)
  putStrLn "divide by 7"
  testWith (7 `asTypeOf` zero)
  putStrLn "divide by 14"
  testWith (14 `asTypeOf` zero)
  putStrLn "divide by 25"
  testWith (25 `asTypeOf` zero)
  putStrLn "divide by maxBound"
  testWith (maxBound `asTypeOf` zero)
  putStrLn "divide by (maxBound - 1)"
  testWith ((maxBound - 1) `asTypeOf` zero)

  when (isSigned zero) $ do
    -- (-1)
    putStrLn "divide by -1"
    testWith ((-1) `asTypeOf` zero)
    
    -- Negative powers of 2
    putStrLn "divide by -2"
    testWith ((-2) `asTypeOf` zero)
    putStrLn "divide by -4"
    testWith ((-4) `asTypeOf` zero)
    
    -- Negative constants
    putStrLn "divide by -3"
    testWith ((-3) `asTypeOf` zero)
    putStrLn "divide by -5"
    testWith ((-5) `asTypeOf` zero)
    putStrLn "divide by -7"
    testWith ((-7) `asTypeOf` zero)
    putStrLn "divide by -14"
    testWith ((-14) `asTypeOf` zero)
    putStrLn "divide by -25"
    testWith ((-25) `asTypeOf` zero)

    -- minBound
    putStrLn "divide by minBound"
    testWith (minBound `asTypeOf` zero)
    putStrLn "divide by (minBound + 1)"
    testWith ((minBound + 1) `asTypeOf` zero)
  where
    testWith d = void $ traverse qr (samples d)
      where
        qr x = do
          print x
          print $ x `quotRem` d
          print $ x `quot` d
          print $ x `rem` d
          putStrLn "#" 
        {-# INLINE qr #-}
    {-# INLINE testWith #-}
    samples d =
         [maxBound]
      ++ plusMinusOne 0
      -- avoid quot minBound
      -- ghc defines quot minBound (-1) = error and has overflow on quot<Int<N>># as undefined behavior
      ++ (if d == (-1) then [] else plusMinusOne largest)
      ++ plusMinusOne large
      ++ plusMinusOne small
      ++ plusMinusOne smallest
      where
        plusMinusOne x = [x - 1, x, x + 1] 
        largest = d * (maxBound `quot` d)
        large = d * ((maxBound `quot` 2) `quot` d)
        small = d * 4
        smallest = d
