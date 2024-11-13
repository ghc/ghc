{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.IO
import GHC.Word

packWord16X8 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Word16X8#
packWord16X8 (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7) = packWord16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)
{-# NOINLINE packWord16X8 #-}

unpackWord16X8 :: Word16X8# -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
unpackWord16X8 v = case unpackWord16X8# v of
  (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7)

arr :: UArray Int Word16
arr = listArray (0,25) [0..25]

indexAsWord16X8 :: UArray Int Word16 -> Int -> Word16X8#
indexAsWord16X8 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord16ArrayAsWord16X8# ba i#

data Word16X8 = Word16X8 Word16X8#

readAsWord16X8 :: IOUArray Int Word16 -> Int -> IO Word16X8
readAsWord16X8 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readWord16ArrayAsWord16X8# mba i# s of
    (# s', v #) -> (# s', Word16X8 v #)

writeAsWord16X8 :: IOUArray Int Word16 -> Int -> Word16X8# -> IO ()
writeAsWord16X8 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeWord16ArrayAsWord16X8# mba i# v s, () #)
{-# NOINLINE writeAsWord16X8 #-}

main :: IO ()
main = do
  let a = packWord16X8# (# 0#Word16, 11#Word16, 22#Word16, 33#Word16, 44#Word16, 55#Word16, 66#Word16, 77#Word16 #)
  print $ unpackWord16X8 a
  print $ unpackWord16X8 (packWord16X8 (33, 22, 11, 0, 77, 66, 55, 44))
  let b = broadcastWord16X8# 65535#Word16
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 0#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 1#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 2#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 3#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 4#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 5#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 6#)
  print $ unpackWord16X8 (insertWord16X8# b 777#Word16 7#)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 0#) == (777, 11, 22, 33, 44, 55, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 1#) == (0, 777, 22, 33, 44, 55, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 2#) == (0, 11, 777, 33, 44, 55, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 3#) == (0, 11, 22, 777, 44, 55, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 4#) == (0, 11, 22, 33, 777, 55, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 5#) == (0, 11, 22, 33, 44, 777, 66, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 6#) == (0, 11, 22, 33, 44, 55, 777, 77)
  print $ unpackWord16X8 (insertWord16X8# a 777#Word16 7#) == (0, 11, 22, 33, 44, 55, 66, 777)
  print $ unpackWord16X8 (indexAsWord16X8 arr 7)
  ma <- newListArray (0, 18) [0..18]
  Word16X8 c <- readAsWord16X8 ma 3
  print $ unpackWord16X8 c
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 0#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 1#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 2#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 3#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 4#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 5#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 6#)
  print =<< getElems ma
  writeAsWord16X8 ma 5 (insertWord16X8# c 0#Word16 7#)
  print =<< getElems ma
  writeAsWord16X8 ma 0 (broadcastWord16X8# 7#Word16)
  print =<< getElems ma
  writeAsWord16X8 ma 6 (packWord16X8# (# 99#Word16, 88#Word16, 77#Word16, 66#Word16, 55#Word16, 44#Word16, 33#Word16, 22#Word16 #))
  print =<< getElems ma
