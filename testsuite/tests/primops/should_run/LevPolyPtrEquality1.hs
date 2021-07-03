{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.IO

data ByteArray = ByteArray ByteArray#

mkTwoByteArrays :: IO ( ByteArray, ByteArray )
mkTwoByteArrays = IO \ s1 -> case newPinnedByteArray# 32# s1 of
  (# s2, mba1 #) -> case unsafeFreezeByteArray# mba1 s2 of
    (# s3, ba1 #) -> case newPinnedByteArray# 32# s3 of
      (# s4, mba2 #) -> case unsafeFreezeByteArray# mba2 s4 of
        (# s5, ba2 #) -> (# s5, ( ByteArray ba1, ByteArray ba2 ) #)

main :: IO ()
main = do
  ( ByteArray ba1, ByteArray ba2 ) <- mkTwoByteArrays
  putStr "eq 1 2: "
  print $ isTrue# ( reallyUnsafePtrEquality# ba1 ba2 )
  putStr "eq 1 1: "
  print $ isTrue# ( reallyUnsafePtrEquality# ba1 ba1 )
