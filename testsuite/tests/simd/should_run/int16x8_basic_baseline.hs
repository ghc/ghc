{-# LANGUAGE MagicHash, UnboxedTuples, LexicalNegation, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.Int
import GHC.IO

packInt16X8 :: (Int16, Int16, Int16, Int16, Int16, Int16, Int16, Int16) -> Int16X8#
packInt16X8 (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7) = packInt16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)
{-# NOINLINE packInt16X8 #-}

unpackInt16X8 :: Int16X8# -> (Int16, Int16, Int16, Int16, Int16, Int16, Int16, Int16)
unpackInt16X8 v = case unpackInt16X8# v of
  (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7)

arr :: UArray Int Int16
arr = listArray (0,25) [0..25]

indexAsInt16X8 :: UArray Int Int16 -> Int -> Int16X8#
indexAsInt16X8 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt16ArrayAsInt16X8# ba i#

data Int16X8 = Int16X8 Int16X8#

readAsInt16X8 :: IOUArray Int Int16 -> Int -> IO Int16X8
readAsInt16X8 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readInt16ArrayAsInt16X8# mba i# s of
    (# s', v #) -> (# s', Int16X8 v #)

writeAsInt16X8 :: IOUArray Int Int16 -> Int -> Int16X8# -> IO ()
writeAsInt16X8 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeInt16ArrayAsInt16X8# mba i# v s, () #)
{-# NOINLINE writeAsInt16X8 #-}

main :: IO ()
main = do
  let a = packInt16X8# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, -4#Int16, 5#Int16, -6#Int16, 7#Int16 #)
  print $ unpackInt16X8 a
  print $ unpackInt16X8 (packInt16X8 (3, 2, 1, 0, -7, 6, -5, -4))
  let b = broadcastInt16X8# 32767#Int16
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 0#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 1#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 2#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 3#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 4#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 5#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 6#)
  print $ unpackInt16X8 (insertInt16X8# b -1#Int16 7#)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 0#) == (-1, 1, 2, 3, -4, 5, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 1#) == (0, -1, 2, 3, -4, 5, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 2#) == (0, 1, -1, 3, -4, 5, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 3#) == (0, 1, 2, -1, -4, 5, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 4#) == (0, 1, 2, 3, -1, 5, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 5#) == (0, 1, 2, 3, -4, -1, -6, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 6#) == (0, 1, 2, 3, -4, 5, -1, 7)
  print $ unpackInt16X8 (insertInt16X8# a -1#Int16 7#) == (0, 1, 2, 3, -4, 5, -6, -1)
  print $ unpackInt16X8 (indexAsInt16X8 arr 7)
  ma <- newListArray (0, 18) [0..18]
  Int16X8 c <- readAsInt16X8 ma 3
  print $ unpackInt16X8 c
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 0#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 1#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 2#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 3#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 4#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 5#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 6#)
  print =<< getElems ma
  writeAsInt16X8 ma 5 (insertInt16X8# c 0#Int16 7#)
  print =<< getElems ma
  writeAsInt16X8 ma 0 (broadcastInt16X8# 7#Int16)
  print =<< getElems ma
  writeAsInt16X8 ma 6 (packInt16X8# (# 9#Int16, -8#Int16, 7#Int16, -6#Int16, 5#Int16, -4#Int16, 3#Int16, -2#Int16 #))
  print =<< getElems ma
