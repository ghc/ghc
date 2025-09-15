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
    case newMutVar# star s0 of
      (# s1, var #) ->
        case readMutVar# var s1 of
          (# s2, U v0 #) ->
            case casMutVar# var star (U 1728#) s2 of
              (# s3, i, U f #) ->
                case casMutVar# var star (U 1989#) s3 of
                  (# s4, j, U g #) ->
                    (# s4, [ I# v0, I# i, I# f, I# j, I# g ] #)
  print res
