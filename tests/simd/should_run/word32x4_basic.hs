{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.IO
import GHC.Word

packWord32X4 :: (Word32, Word32, Word32, Word32) -> Word32X4#
packWord32X4 (W32# x0, W32# x1, W32# x2, W32# x3) = packWord32X4# (# x0, x1, x2, x3 #)
{-# NOINLINE packWord32X4 #-}

unpackWord32X4 :: Word32X4# -> (Word32, Word32, Word32, Word32)
unpackWord32X4 v = case unpackWord32X4# v of
  (# x0, x1, x2, x3 #) -> (W32# x0, W32# x1, W32# x2, W32# x3)

arr :: UArray Int Word32
arr = listArray (0,9) [0,1,2,3,4,5,6,7,8,9]

indexAsWord32X4 :: UArray Int Word32 -> Int -> Word32X4#
indexAsWord32X4 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord32ArrayAsWord32X4# ba i#

data Word32X4 = Word32X4 Word32X4#

readAsWord32X4 :: IOUArray Int Word32 -> Int -> IO Word32X4
readAsWord32X4 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readWord32ArrayAsWord32X4# mba i# s of
    (# s', v #) -> (# s', Word32X4 v #)

writeAsWord32X4 :: IOUArray Int Word32 -> Int -> Word32X4# -> IO ()
writeAsWord32X4 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeWord32ArrayAsWord32X4# mba i# v s, () #)
{-# NOINLINE writeAsWord32X4 #-}

main :: IO ()
main = do
  let a = packWord32X4# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32 #)
  print $ unpackWord32X4 a
  print $ unpackWord32X4 (packWord32X4 (3, 2, 1, 0))
  let b = broadcastWord32X4# 5#Word32
  print $ unpackWord32X4 (insertWord32X4# b 7#Word32 0#)
  print $ unpackWord32X4 (insertWord32X4# b 7#Word32 1#)
  print $ unpackWord32X4 (insertWord32X4# b 7#Word32 2#)
  print $ unpackWord32X4 (insertWord32X4# b 7#Word32 3#)
  print $ unpackWord32X4 (insertWord32X4# a 42#Word32 0#) == (42, 1, 2, 3)
  print $ unpackWord32X4 (insertWord32X4# a 42#Word32 1#) == (0, 42, 2, 3)
  print $ unpackWord32X4 (insertWord32X4# a 42#Word32 2#) == (0, 1, 42, 3)
  print $ unpackWord32X4 (insertWord32X4# a 42#Word32 3#) == (0, 1, 2, 42)
  print $ unpackWord32X4 (indexAsWord32X4 arr 3)
  ma <- newListArray (0, 9) [0..9]
  Word32X4 c <- readAsWord32X4 ma 3
  print $ unpackWord32X4 c
  writeAsWord32X4 ma 5 (insertWord32X4# c 0#Word32 0#)
  print =<< getElems ma
  writeAsWord32X4 ma 5 (insertWord32X4# c 0#Word32 1#)
  print =<< getElems ma
  writeAsWord32X4 ma 5 (insertWord32X4# c 0#Word32 2#)
  print =<< getElems ma
  writeAsWord32X4 ma 5 (insertWord32X4# c 0#Word32 3#)
  print =<< getElems ma
  writeAsWord32X4 ma 0 (broadcastWord32X4# 7#Word32)
  print =<< getElems ma
  writeAsWord32X4 ma 6 (packWord32X4# (# 9999#Word32, 8888#Word32, 7777#Word32, 6666#Word32 #))
  print =<< getElems ma
