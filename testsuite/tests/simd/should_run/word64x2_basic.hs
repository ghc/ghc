{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.IO
import GHC.Word

packWord64X2 :: (Word64, Word64) -> Word64X2#
packWord64X2 (W64# x0, W64# x1) = packWord64X2# (# x0, x1 #)
{-# NOINLINE packWord64X2 #-}

unpackWord64X2 :: Word64X2# -> (Word64, Word64)
unpackWord64X2 v = case unpackWord64X2# v of
  (# x0, x1 #) -> (W64# x0, W64# x1)

arr :: UArray Int Word64
arr = listArray (0,9) [0,1111,2222,3333,4444,5555,6666,7777,8888,999]

indexAsWord64X2 :: UArray Int Word64 -> Int -> Word64X2#
indexAsWord64X2 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexWord64ArrayAsWord64X2# ba i#

data Word64X2 = Word64X2 Word64X2#

readAsWord64X2 :: IOUArray Int Word64 -> Int -> IO Word64X2
readAsWord64X2 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readWord64ArrayAsWord64X2# mba i# s of
    (# s', v #) -> (# s', Word64X2 v #)

writeAsWord64X2 :: IOUArray Int Word64 -> Int -> Word64X2# -> IO ()
writeAsWord64X2 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeWord64ArrayAsWord64X2# mba i# v s, () #)
{-# NOINLINE writeAsWord64X2 #-}

main :: IO ()
main = do
  let a = packWord64X2# (# 0#Word64, 1#Word64 #)
  print $ unpackWord64X2 a
  print $ unpackWord64X2 (packWord64X2 (3, 2))
  let b = broadcastWord64X2# 5#Word64
  print $ unpackWord64X2 (insertWord64X2# b 7#Word64 0#)
  print $ unpackWord64X2 (insertWord64X2# b 7#Word64 1#)
  print $ unpackWord64X2 (insertWord64X2# a 9#Word64 0#) == (9, 1)
  print $ unpackWord64X2 (insertWord64X2# a 9#Word64 1#) == (0, 9)
  print $ unpackWord64X2 (indexAsWord64X2 arr 3)
  ma <- newListArray (0, 9) [0..9]
  Word64X2 c <- readAsWord64X2 ma 3
  print $ unpackWord64X2 c
  writeAsWord64X2 ma 5 (insertWord64X2# c 0#Word64 0#)
  print =<< getElems ma
  writeAsWord64X2 ma 5 (insertWord64X2# c 0#Word64 1#)
  print =<< getElems ma
  writeAsWord64X2 ma 0 (broadcastWord64X2# 7#Word64)
  print =<< getElems ma
  writeAsWord64X2 ma 6 (packWord64X2# (# 99#Word64, 88#Word64 #))
  print =<< getElems ma
