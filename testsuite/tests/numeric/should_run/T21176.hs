module Main where

import Data.Bits
import Data.Int (Int32, Int64)
import Data.Foldable (for_)
import GHC.Num.Integer (integerCheck)

integers :: [Integer]
integers = concatMap neighbours [minInt64, minInt32, 0, maxInt32, maxInt64]
  where
    neighbours i = [i - 2, i - 1, i, i + 1, i + 2]
    minInt64 = toInteger (minBound :: Int64)
    minInt32 = toInteger (minBound :: Int32)
    maxInt32 = toInteger (maxBound :: Int32)
    maxInt64 = toInteger (maxBound :: Int64)

bits :: [Int]
bits = [0, 1, x - 1, x, x + 1]
  where x = finiteBitSize (0 :: Int) - 1

testXBit :: String -> (Integer -> Int -> Integer) -> (Integer -> Int -> Integer) -> IO ()
testXBit name f model = do
  putStrLn name
  for_ integers $ \i ->
    for_ bits $ \b -> do
      let actual   = f i b
          expected = model i b
          valid    = if integerCheck actual then "valid"   else "invalid"
          matches  = if actual == expected  then "matches" else "differs"
      putStrLn $ "  " ++ show i ++ " " ++ show b ++ " -> " ++ show actual
              ++ "  [" ++ valid ++ ", " ++ matches ++ "]"
  putStrLn ""

main :: IO ()
main = do
  testXBit "setBit"        setBit        (\i b -> i .|. bit b)
  testXBit "clearBit"      clearBit      (\i b -> i .&. complement (bit b))
  testXBit "complementBit" complementBit (\i b -> i `xor` bit b)
