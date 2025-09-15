{-# LANGUAGE MagicHash, UnboxedTuples, LexicalNegation, ExtendedLiterals #-}
import Data.Array.Base
import Data.Array.IO.Internals
import GHC.Exts
import GHC.Int
import GHC.IO

packInt32X4 :: (Int32, Int32, Int32, Int32) -> Int32X4#
packInt32X4 (I32# x0, I32# x1, I32# x2, I32# x3) = packInt32X4# (# x0, x1, x2, x3 #)
{-# NOINLINE packInt32X4 #-}

unpackInt32X4 :: Int32X4# -> (Int32, Int32, Int32, Int32)
unpackInt32X4 v = case unpackInt32X4# v of
  (# x0, x1, x2, x3 #) -> (I32# x0, I32# x1, I32# x2, I32# x3)

arr :: UArray Int Int32
arr = listArray (0,9) [0,1,2,3,4,5,6,7,8,9]

indexAsInt32X4 :: UArray Int Int32 -> Int -> Int32X4#
indexAsInt32X4 (UArray l _ _ ba) i = case i - l of
  I# i# -> indexInt32ArrayAsInt32X4# ba i#

data Int32X4 = Int32X4 Int32X4#

readAsInt32X4 :: IOUArray Int Int32 -> Int -> IO Int32X4
readAsInt32X4 (IOUArray (STUArray l _ _ mba)) i = case i - l of
  I# i# -> IO $ \s -> case readInt32ArrayAsInt32X4# mba i# s of
    (# s', v #) -> (# s', Int32X4 v #)

writeAsInt32X4 :: IOUArray Int Int32 -> Int -> Int32X4# -> IO ()
writeAsInt32X4 (IOUArray (STUArray l _ _ mba)) i v = case i - l of
  I# i# -> IO $ \s -> (# writeInt32ArrayAsInt32X4# mba i# v s, () #)
{-# NOINLINE writeAsInt32X4 #-}

main :: IO ()
main = do
  let a = packInt32X4# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32 #)
  print $ unpackInt32X4 a
  print $ unpackInt32X4 (packInt32X4 (3, 2, 1, 0))
  let b = broadcastInt32X4# 5#Int32
  print $ unpackInt32X4 (insertInt32X4# b 7#Int32 0#)
  print $ unpackInt32X4 (insertInt32X4# b 7#Int32 1#)
  print $ unpackInt32X4 (insertInt32X4# b 7#Int32 2#)
  print $ unpackInt32X4 (insertInt32X4# b 7#Int32 3#)
  print $ unpackInt32X4 (insertInt32X4# a -1#Int32 0#) == (-1, 1, 2, 3)
  print $ unpackInt32X4 (insertInt32X4# a -1#Int32 1#) == (0, -1, 2, 3)
  print $ unpackInt32X4 (insertInt32X4# a -1#Int32 2#) == (0, 1, -1, 3)
  print $ unpackInt32X4 (insertInt32X4# a -1#Int32 3#) == (0, 1, 2, -1)
  print $ unpackInt32X4 (indexAsInt32X4 arr 3)
  ma <- newListArray (0, 9) [0..9]
  Int32X4 c <- readAsInt32X4 ma 3
  print $ unpackInt32X4 c
  writeAsInt32X4 ma 5 (insertInt32X4# c 0#Int32 0#)
  print =<< getElems ma
  writeAsInt32X4 ma 5 (insertInt32X4# c 0#Int32 1#)
  print =<< getElems ma
  writeAsInt32X4 ma 5 (insertInt32X4# c 0#Int32 2#)
  print =<< getElems ma
  writeAsInt32X4 ma 5 (insertInt32X4# c 0#Int32 3#)
  print =<< getElems ma
  writeAsInt32X4 ma 0 (broadcastInt32X4# 7#Int32)
  print =<< getElems ma
  writeAsInt32X4 ma 6 (packInt32X4# (# 9#Int32, -8#Int32, 7#Int32, 6#Int32 #))
  print =<< getElems ma
