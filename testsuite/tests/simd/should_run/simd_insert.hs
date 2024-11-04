{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Exts

unpackFloatX4 :: FloatX4# -> (Float, Float, Float, Float)
unpackFloatX4 v = case unpackFloatX4# v of
  (# a0, a1, a2, a3 #) -> (F# a0, F# a1, F# a2, F# a3)

unpackDoubleX2 :: DoubleX2# -> (Double, Double)
unpackDoubleX2 v = case unpackDoubleX2# v of
  (# a0, a1 #) -> (D# a0, D# a1)

testFloatX4 :: IO ()
testFloatX4 = do
  let v = packFloatX4# (# 0.1#, 1.0#, 2.0#, 3.0# #)
  print $ unpackFloatX4 v
  let w = insertFloatX4# v 7.0# 0#
  print $ unpackFloatX4 w
  let x = insertFloatX4# v 7.0# 1#
  print $ unpackFloatX4 x
  let y = insertFloatX4# v 7.0# 2#
  print $ unpackFloatX4 y
  let z = insertFloatX4# v 7.0# 3#
  print $ unpackFloatX4 z

testDoubleX2 :: IO ()
testDoubleX2 = do
  let v = packDoubleX2# (# 0.1##, 1.0## #)
  print $ unpackDoubleX2 v
  let w = insertDoubleX2# v 7.0## 0#
  print $ unpackDoubleX2 w
  let x = insertDoubleX2# v 7.0## 1#
  print $ unpackDoubleX2 x

main :: IO ()
main = do
  testFloatX4
  testDoubleX2
