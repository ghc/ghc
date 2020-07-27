{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts
import GHC.IO

main :: IO ()
main = IO $ \s -> case newByteArray# 1032161# s of
  (# s', mba# #) -> case unpackClosure# (unsafeCoerce# mba# :: Any) of
    (# !_, _, _ #) -> (# s', () #)
