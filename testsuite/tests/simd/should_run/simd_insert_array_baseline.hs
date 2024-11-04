{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Data.Array.Base
import GHC.Exts

unpackFloatX4 :: FloatX4# -> (Float, Float, Float, Float)
unpackFloatX4 v = case unpackFloatX4# v of
  (# a0, a1, a2, a3 #) -> (F# a0, F# a1, F# a2, F# a3)

unpackDoubleX2 :: DoubleX2# -> (Double, Double)
unpackDoubleX2 v = case unpackDoubleX2# v of
  (# a0, a1 #) -> (D# a0, D# a1)

indexFloatArrayAsFloatX4 :: UArray Int Float -> Int -> FloatX4#
indexFloatArrayAsFloatX4 (UArray l u n ba) i = case i - l of I# i# -> indexFloatArrayAsFloatX4# ba i#

indexDoubleArrayAsDoubleX2 :: UArray Int Double -> Int -> DoubleX2#
indexDoubleArrayAsDoubleX2 (UArray l u n ba) i = case i - l of I# i# -> indexDoubleArrayAsDoubleX2# ba i#

someFloatArray :: UArray Int Float
someFloatArray = listArray (0, 7) [111.0, 222.0, 333.0, 444.0, 555.0, 666.0, 777.0, 888.0]

someDoubleArray :: UArray Int Double
someDoubleArray = listArray (0, 7) [111.0, 222.0, 333.0, 444.0, 555.0, 666.0, 777.0, 888.0]

testFloatX4 :: IO ()
testFloatX4 = forM_ [0,4] $ \i -> do
  let v = indexFloatArrayAsFloatX4 someFloatArray i
  let w = insertFloatX4# v 123.45# 0#
  print $ unpackFloatX4 w
  let x = insertFloatX4# v 123.45# 1#
  print $ unpackFloatX4 x
  let y = insertFloatX4# v 123.45# 2#
  print $ unpackFloatX4 y
  let z = insertFloatX4# v 123.45# 3#
  print $ unpackFloatX4 z

testDoubleX2 :: IO ()
testDoubleX2 = forM_ [0,2,4,6] $ \i -> do
  let v = indexDoubleArrayAsDoubleX2 someDoubleArray i
  let w = insertDoubleX2# v 123.45## 0#
  print $ unpackDoubleX2 w
  let x = insertDoubleX2# v 123.45## 1#
  print $ unpackDoubleX2 x

main :: IO ()
main = do
  testFloatX4
  testDoubleX2
