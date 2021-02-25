{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnboxedTuples, UnliftedFFITypes #-}
import GHC.Exts

foreign import prim "someFuncF"
  someFuncF :: Float# -> Float# -> Float# -> Float# -> (# Float#, Float#, Float#, Float# #)

foreign import prim "someFuncD"
  someFuncD :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double#, Double#, Double# #)

{-
someFuncF :: Float# -> Float# -> Float# -> Float# -> (# Float#, Float#, Float#, Float# #)
someFuncF x y z w = (# x `plusFloat#` y, x `minusFloat#` y, z `timesFloat#` w, z `divideFloat#` w #)

someFuncD :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double#, Double#, Double# #)
someFuncD x y z w = (# x +## y, x -## y, z *## w, z /## w #)
-}

main = do
  case someFuncF 1.0# 3.0# 4.0# 2.0# of
    (# a, b, c, d #) -> do
      print (F# a)
      print (F# b)
      print (F# c)
      print (F# d)
  case someFuncD 1.0## 3.0## 4.0## 2.0## of
    (# a, b, c, d #) -> do
      print (D# a)
      print (D# b)
      print (D# c)
      print (D# d)
