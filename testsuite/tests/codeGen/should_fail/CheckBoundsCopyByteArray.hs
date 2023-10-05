{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newByteArray# 4# s0 of
        (# s1, src_marr #) ->
          case newByteArray# 4# s1 of
            (# s2, dst_marr #) ->
              case unsafeFreezeByteArray# src_marr s2 of
                (# s3, src_arr #) ->
                  case copyByteArray# src_arr 0# dst_marr 1# 4# s3 of
                    s4 -> (# s4, () #)

