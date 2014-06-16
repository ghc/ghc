module Main where

import Data.Bits
import Data.Word

test_and1 :: Word -> Word
test_and1 x = x .&. 0

test_and2 :: Word -> Word
test_and2 x = 0 .&. x

test_or1 :: Word -> Word
test_or1 x = x .|. 0

test_or2 :: Word -> Word
test_or2 x = 0 .|. x

test_shiftL :: Word -> Word
test_shiftL x = shiftL x 0

test_shiftR :: Word -> Word
test_shiftR x = shiftR x 0

test_add1 :: Int -> Int
test_add1 x = x + 0

test_add2 :: Int -> Int
test_add2 x = 0 + x

test_sub1 :: Int -> Int
test_sub1 x = x - 0

test_sub2 :: Int -> Int
test_sub2 x = x - x

test_mul1 :: Int -> Int
test_mul1 x = x * 1

test_mul2 :: Int -> Int
test_mul2 x = 1 * x

test_mul3 :: Int -> Int
test_mul3 x = x * 0

test_quot :: Int -> Int
test_quot x = x `quot` 1

test_rem :: Int -> Int
test_rem x = x `rem` 1

test_addf1 :: Float -> Float
test_addf1 x = x + 0

test_addf2 :: Float -> Float
test_addf2 x = 0 + x

test_subf :: Float -> Float
test_subf x = x - 0

test_mulf1 :: Float -> Float
test_mulf1 x = x * 1

test_mulf2 :: Float -> Float
test_mulf2 x = 1 * x

test_divf :: Float -> Float
test_divf x = x / 1

main :: IO ()
main = do
  print $ test_and1 42
  print $ test_and2 43
  print $ test_or1 44
  print $ test_or2 45
  print $ test_shiftL 46
  print $ test_shiftR 47
  print $ test_add1 48
  print $ test_add2 49
  print $ test_sub1 50
  print $ test_sub2 51
  print $ test_mul1 52
  print $ test_mul2 53
  print $ test_mul3 54
  print $ test_quot 55
  print $ test_rem 56
  print $ test_addf1 57
  print $ test_addf2 58
  print $ test_subf 59
  print $ test_mulf1 60
  print $ test_mulf2 61
  print $ test_divf 62
