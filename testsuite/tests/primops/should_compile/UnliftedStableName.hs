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
data U = U Int# Int#

main :: IO ()
main = do
  IO \ s0 ->
    case makeStableName# (U 97531# 86420#) s0 of
      (# s1, nm1 #) ->
        case makeStableName# (U 86420# 97531#) s1 of
          (# s2, nm2 #) ->
            case makeStableName# (U 97531# 86420#) s1 of
              (# s3, nm3 #) ->
                (# s3, () #)
