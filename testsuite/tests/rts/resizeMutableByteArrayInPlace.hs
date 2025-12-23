{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Monad
import GHC.Exts
import GHC.IO

-- Given newByteArray#/newPinnedByteArray#, iterate given number of
-- rounds: first allocate a MutableByteArray# using the first size,
-- then resize to the new size, then resize back
{-# INLINE testResize #-}
testResize :: (Int# -> State# RealWorld -> (# State# RealWorld, MutableByteArray# RealWorld #)) -> Int -> Int -> Int -> IO ()
testResize alloc# rounds (I# sz0#) (I# sz1#) =
  replicateM_ rounds $ IO $ \s0 -> case alloc# sz0# s0 of
    (# s1, mba0# #) -> case resizeMutableByteArray# mba0# sz1# s1 of
      (# s2, mba1# #) -> case resizeMutableByteArray# mba1# sz0# s2 of
        (# s3, _ #) -> (# s3, () #)

main :: IO ()
main = do
  testResize newByteArray# 100000 8 64
  testResize newByteArray# 100000 64 8
  testResize newPinnedByteArray# 100000 8 64
  testResize newPinnedByteArray# 100000 64 8
