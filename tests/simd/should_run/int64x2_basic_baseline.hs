{-# LANGUAGE MagicHash, UnboxedTuples, LexicalNegation, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.Int
import GHC.IO

packInt64X2 :: (Int64, Int64) -> Int64X2#
packInt64X2 (I64# x0, I64# x1) = packInt64X2# (# x0, x1 #)
{-# NOINLINE packInt64X2 #-}

unpackInt64X2 :: Int64X2# -> (Int64, Int64)
unpackInt64X2 v = case unpackInt64X2# v of
  (# x0, x1 #) -> (I64# x0, I64# x1)

arr :: UArray Int Int64
arr = listArray (0,9) [0,1,2,3,4,5,6,7,8,9]

indexAsInt64X2 :: UArray Int Int64 -> Int -> Int64X2#
indexAsInt64X2 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt64ArrayAsInt64X2# ba i#

data Int64X2 = Int64X2 Int64X2#

readAsInt64X2 :: IOUArray Int Int64 -> Int -> IO Int64X2
readAsInt64X2 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readInt64ArrayAsInt64X2# mba i# s of
    (# s', v #) -> (# s', Int64X2 v #)

writeAsInt64X2 :: IOUArray Int Int64 -> Int -> Int64X2# -> IO ()
writeAsInt64X2 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeInt64ArrayAsInt64X2# mba i# v s, () #)
{-# NOINLINE writeAsInt64X2 #-}

main :: IO ()
main = do
  let a = packInt64X2# (# 0#Int64, 1#Int64 #)
  print $ unpackInt64X2 a
  print $ unpackInt64X2 (packInt64X2 (3, 2))
  let b = broadcastInt64X2# 5#Int64
  print $ unpackInt64X2 (insertInt64X2# b 7#Int64 0#)
  print $ unpackInt64X2 (insertInt64X2# b 7#Int64 1#)
  print $ unpackInt64X2 (insertInt64X2# a -1#Int64 0#) == (-1, 1)
  print $ unpackInt64X2 (insertInt64X2# a -1#Int64 1#) == (0, -1)
  print $ unpackInt64X2 (indexAsInt64X2 arr 3)
  ma <- newListArray (0, 9) [0..9]
  Int64X2 c <- readAsInt64X2 ma 3
  print $ unpackInt64X2 c
  writeAsInt64X2 ma 5 (insertInt64X2# c 0#Int64 0#)
  print =<< getElems ma
  writeAsInt64X2 ma 5 (insertInt64X2# c 0#Int64 1#)
  print =<< getElems ma
  writeAsInt64X2 ma 0 (broadcastInt64X2# 7#Int64)
  print =<< getElems ma
  writeAsInt64X2 ma 6 (packInt64X2# (# 9#Int64, -8#Int64 #))
  print =<< getElems ma
