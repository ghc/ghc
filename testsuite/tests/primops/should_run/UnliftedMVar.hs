{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Data.Kind
import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = U Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newMVar# s0 of
      (# s1, mvar #) ->
        case tryTakeMVar# mvar s1 of
          (# s2, i, _ #) ->
            case putMVar# mvar (U 1612#) s2 of
              s3 ->
                case readMVar# mvar s3 of
                  (# s4, U r1 #) ->
                    case takeMVar# mvar s4 of
                      (# s5, U r2 #) ->
                        case tryReadMVar# mvar s5 of
                          (# s6, j, _ #) ->
                            case isEmptyMVar# mvar s6 of
                              (# s7, k #) ->
                                (# s6, [ I# i, I# r1, I# r2, I# j, I# k ] #)
  print res
