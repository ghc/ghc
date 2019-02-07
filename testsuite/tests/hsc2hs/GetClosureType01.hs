{-# LINE 1 "GetClosureType01.hsc" #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Main (main) where

import GHC.IO (IO(..))
import GHC.Exts
import Control.Monad

data MutableByteArray = MutableByteArray (MutableByteArray# RealWorld)

main :: IO ()
main = do
  MutableByteArray x <- allocateBytes
  let expected = 53
  let actual = W# (getClosureType# (unsafeCoerce# x))
  when (expected /= actual) $ fail
    ("Expected closure type " ++ show expected ++ " but got " ++ show actual)

allocateBytes :: IO MutableByteArray
allocateBytes = IO $ \s0 -> case newByteArray# 1# s0 of
  (# s1, arr #) -> (# s1, MutableByteArray arr #)

