{-# language MagicHash, UnboxedTuples, UnboxedSums #-}

module Main ( main ) where

import GHC.Exts
import GHC.Int
import GHC.Word
import GHC.Float

foo :: (# Int64X2# | Bool | DoubleX2# #)
    -> (# Integer | (# FloatX4#, Int64#, Int64# #) | Char #)
foo (# i64x2 | | #) =
  case unpackInt64X2# i64x2 of
    (# i1, i2 #) ->
      let
        s = sum $ map fromIntegral
             [ I64# i1, I64# i2 ]
      in (# s | | #)

foo (# | b | #) = if b then (# 0 | | #) else (# | | 'F' #)
foo (# | | dx2 #) =
  case unpackDoubleX2# dx2 of
    (# d1, d2 #) ->
      let (# m1, e1 #) = decodeDouble_Int64# d1
          (# m2, e2 #) = decodeDouble_Int64# d2
          v = packFloatX4#
                (# double2Float# d1
                ,  int2Float#    e1
                ,  double2Float# d2
                ,  int2Float#    e2 #)
      in (# | (# v, m1, m2 #) | #)

show_it :: (# Integer | (# FloatX4#, Int64#, Int64# #) | Char #) -> String
show_it (# i | | #) = "(# " ++ show i ++ " | | #)"
show_it (# | (# fx4, m1, m2 #) | #) = "(# | (# " ++ showFloatX4 fx4 ++ ", " ++ show (I64# m1) ++ ", " ++ show (I64# m2) ++ " #) | #)"
show_it (# | | c #) = "(# | | " ++ show c ++ " #)"

showFloatX4 :: FloatX4# -> String
showFloatX4 fx4 = case unpackFloatX4# fx4 of
  (# f1, f2, f3, f4 #) ->
    "(# " ++ show (F# f1) ++ ", " ++ show (F# f2) ++ ", "
          ++ show (F# f3) ++ ", " ++ show (F# f4) ++ " #)"

main :: IO ()
main = do
  putStrLn $ show_it ( foo (# broadcastInt64X2# ( intToInt64# 1# ) | | #) )
  putStrLn $ show_it ( foo (# | False | #) )
  putStrLn $ show_it ( foo (# | True | #) )
  let dx2 = packDoubleX2# (# 128.0##, -0.0025## #)
  putStrLn $ show_it ( foo (# | | dx2 #) )
