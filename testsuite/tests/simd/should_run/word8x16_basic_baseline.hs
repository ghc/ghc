{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.IO
import GHC.Word

packWord8X16 :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Word8X16#
packWord8X16 (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15) = packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)
{-# NOINLINE packWord8X16 #-}

unpackWord8X16AsList :: Word8X16# -> [Word8]
unpackWord8X16AsList v = case unpackWord8X16# v of
  (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> [W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15]

arr :: UArray Int Word8
arr = listArray (0,25) [0..25]

indexAsWord8X16 :: UArray Int Word8 -> Int -> Word8X16#
indexAsWord8X16 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord8ArrayAsWord8X16# ba i#

data Word8X16 = Word8X16 Word8X16#

readAsWord8X16 :: IOUArray Int Word8 -> Int -> IO Word8X16
readAsWord8X16 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readWord8ArrayAsWord8X16# mba i# s of
    (# s', v #) -> (# s', Word8X16 v #)

writeAsWord8X16 :: IOUArray Int Word8 -> Int -> Word8X16# -> IO ()
writeAsWord8X16 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeWord8ArrayAsWord8X16# mba i# v s, () #)
{-# NOINLINE writeAsWord8X16 #-}

main :: IO ()
main = do
  let a = packWord8X16# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 8#Word8, 9#Word8, 10#Word8, 11#Word8, 12#Word8, 13#Word8, 14#Word8, 15#Word8 #)
  print $ unpackWord8X16AsList a
  print $ unpackWord8X16AsList (packWord8X16 (3, 2, 1, 0, 7, 6, 5, 4, 15, 14, 13, 12, 11, 10, 9, 8))
  let b = broadcastWord8X16# 255#Word8
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 0#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 1#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 2#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 3#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 4#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 5#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 6#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 7#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 8#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 9#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 10#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 11#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 12#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 13#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 14#)
  print $ unpackWord8X16AsList (insertWord8X16# b 77#Word8 15#)
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 0#) == [77, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 1#) == [0, 77, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 2#) == [0, 1, 77, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 3#) == [0, 1, 2, 77, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 4#) == [0, 1, 2, 3, 77, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 5#) == [0, 1, 2, 3, 4, 77, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 6#) == [0, 1, 2, 3, 4, 5, 77, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 7#) == [0, 1, 2, 3, 4, 5, 6, 77, 8, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 8#) == [0, 1, 2, 3, 4, 5, 6, 7, 77, 9, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 9#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 77, 10, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 10#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 77, 11, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 11#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 77, 12, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 12#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 77, 13, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 13#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 77, 14, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 14#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 77, 15]
  print $ unpackWord8X16AsList (insertWord8X16# a 77#Word8 15#) == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 77]
  print $ unpackWord8X16AsList (indexAsWord8X16 arr 7)
  ma <- newListArray (0, 25) [0..25]
  Word8X16 c <- readAsWord8X16 ma 3
  print $ unpackWord8X16AsList c
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 0#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 1#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 2#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 3#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 4#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 5#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 6#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 7#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 8#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 9#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 10#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 11#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 12#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 13#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 14#)
  print =<< getElems ma
  writeAsWord8X16 ma 5 (insertWord8X16# c 0#Word8 15#)
  print =<< getElems ma
  writeAsWord8X16 ma 0 (broadcastWord8X16# 7#Word8)
  print =<< getElems ma
  writeAsWord8X16 ma 6 (packWord8X16# (# 99#Word8, 88#Word8, 77#Word8, 66#Word8, 55#Word8, 44#Word8, 33#Word8, 22#Word8, 11#Word8, 0#Word8, 100#Word8, 110#Word8, 120#Word8, 130#Word8, 140#Word8, 150#Word8 #))
  print =<< getElems ma
