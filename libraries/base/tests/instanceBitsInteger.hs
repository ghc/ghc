module Main where

import Control.Monad (when)
import Data.Bits
import Data.Int (Int32, Int64)
import Data.Foldable (for_)
import GHC.Num.Integer (integerCheck)

integers :: [Integer]
integers = concatMap f [minInt64, minInt32, 0, maxInt32, maxInt64]
  where
    f i = [pred $ pred i, pred i, i, succ i, succ $ succ i]
    minInt64 = toInteger (minBound :: Int64)
    minInt32 = toInteger (minBound :: Int32)
    maxInt32 = toInteger (maxBound :: Int32)
    maxInt64 = toInteger (maxBound :: Int64)

bits :: [Int]
bits = [0,1,62,63,64]

testXBit name f model = do
  putStrLn name
  for_ integers $ \i ->
    for_ bits $ \b -> do
      let actual = f i b
      let expected = model i b
      putStr $ name ++ " (" ++ show i ++ ") " ++ show b ++ " = " ++ show actual
      putStr ", "
      putStr $ if integerCheck actual then "valid Integer" else "invalid Integer"
      putStr ", "
      putStrLn $ if actual == expected then "matches default implementation" else "doesn't match default implementation"
  putStrLn ""

main :: IO ()
main = do
  testXBit "setBit" setBit (\i b -> i .|. bit b)
  testXBit "clearBit" clearBit (\i b -> i .&. complement (bit b))
  testXBit "complementBit" complementBit (\i b -> i `xor` bit b)
