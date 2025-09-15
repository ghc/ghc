{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Data.Kind
import System.Mem (performGC)
import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = U Int# Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    let u :: U
        u = U 97531# 86420#
    in
      case makeStablePtr# u s0 of
        (# s1, ptr #) ->
          case unIO performGC s1 of
            (# s3, _ #) ->
              case deRefStablePtr# ptr s3 of
                (# s4, U i j #) ->
                  case makeStablePtr# (U 123# 456#) s4 of
                    (# s5, ptr' #) ->
                      (# s5, [ I# i, I# j, I# (eqStablePtr# ptr ptr), I# (eqStablePtr# ptr ptr') ] #)
  print res
