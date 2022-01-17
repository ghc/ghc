{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = U Int#

main :: IO ()
main = do
  let star = U 1612#
  res <- IO \ s0 ->
    case newArray# 10# star s0 of
      (# s1, arr #) ->
        case readArray# arr 7# s1 of
          (# s2, U v0 #) ->
            case casArray# arr 7# star (U 1728#) s2 of
              (# s2, i, U f #) ->
                case casArray# arr 7# star (U 1989#) s2 of
                  (# s3, j, U g #) ->
                    (# s3, [ I# v0, I# i, I# f, I# j, I# g ] #)
  print res
