{-# LANGUAGE MagicHash, UnboxedTuples, LexicalNegation, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.Int
import GHC.IO

packInt8X16 :: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8) -> Int8X16#
packInt8X16 (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15) = packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)
{-# NOINLINE packInt8X16 #-}

unpackInt8X16AsList :: Int8X16# -> [Int8]
unpackInt8X16AsList v = case unpackInt8X16# v of
  (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> [I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15]

arr :: UArray Int Int8
arr = listArray (0,25) [0..25]

indexAsInt8X16 :: UArray Int Int8 -> Int -> Int8X16#
indexAsInt8X16 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt8ArrayAsInt8X16# ba i#

data Int8X16 = Int8X16 Int8X16#

readAsInt8X16 :: IOUArray Int Int8 -> Int -> IO Int8X16
readAsInt8X16 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readInt8ArrayAsInt8X16# mba i# s of
    (# s', v #) -> (# s', Int8X16 v #)

writeAsInt8X16 :: IOUArray Int Int8 -> Int -> Int8X16# -> IO ()
writeAsInt8X16 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeInt8ArrayAsInt8X16# mba i# v s, () #)
{-# NOINLINE writeAsInt8X16 #-}

main :: IO ()
main = do
  let a = packInt8X16# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, -4#Int8, 5#Int8, -6#Int8, 7#Int8, -8#Int8, 9#Int8, -10#Int8, 11#Int8, -12#Int8, 13#Int8, -14#Int8, 15#Int8 #)
  print $ unpackInt8X16AsList a
  print $ unpackInt8X16AsList (packInt8X16 (3, 2, 1, 0, -7, 6, -5, -4, 15, -14, 13, -12, 11, -10, 9, -8))
  let b = broadcastInt8X16# 127#Int8
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 0#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 1#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 2#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 3#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 4#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 5#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 6#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 7#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 8#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 9#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 10#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 11#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 12#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 13#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 14#)
  print $ unpackInt8X16AsList (insertInt8X16# b -1#Int8 15#)
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 0#) == [-1, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 1#) == [0, -1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 2#) == [0, 1, -1, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 3#) == [0, 1, 2, -1, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 4#) == [0, 1, 2, 3, -1, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 5#) == [0, 1, 2, 3, -4, -1, -6, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 6#) == [0, 1, 2, 3, -4, 5, -1, 7, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 7#) == [0, 1, 2, 3, -4, 5, -6, -1, -8, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 8#) == [0, 1, 2, 3, -4, 5, -6, 7, -1, 9, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 9#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, -1, -10, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 10#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -1, 11, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 11#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, -1, -12, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 12#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -1, 13, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 13#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, -1, -14, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 14#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -1, 15]
  print $ unpackInt8X16AsList (insertInt8X16# a -1#Int8 15#) == [0, 1, 2, 3, -4, 5, -6, 7, -8, 9, -10, 11, -12, 13, -14, -1]
  print $ unpackInt8X16AsList (indexAsInt8X16 arr 7)
  ma <- newListArray (0, 25) [0..25]
  Int8X16 c <- readAsInt8X16 ma 3
  print $ unpackInt8X16AsList c
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 0#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 1#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 2#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 3#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 4#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 5#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 6#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 7#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 8#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 9#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 10#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 11#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 12#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 13#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 14#)
  print =<< getElems ma
  writeAsInt8X16 ma 5 (insertInt8X16# c 0#Int8 15#)
  print =<< getElems ma
  writeAsInt8X16 ma 0 (broadcastInt8X16# 7#Int8)
  print =<< getElems ma
  writeAsInt8X16 ma 6 (packInt8X16# (# 9#Int8, -8#Int8, 7#Int8, -6#Int8, 5#Int8, -4#Int8, 3#Int8, -2#Int8, 1#Int8, 0#Int8, -10#Int8, 11#Int8, -12#Int8, 13#Int8, -14#Int8, 15#Int8 #))
  print =<< getElems ma
