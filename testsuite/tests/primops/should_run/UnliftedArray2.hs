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
data U = U !Int

main :: IO ()
main = do
  res <- IO \ s1 ->
    case newArray# 7# (U 3) s1 of
      (# s2, marr1 #) ->
        case newArray# 9# (U 8) s2 of
          (# s3, marr2 #) ->
            case copyMutableArray# marr1 2# marr2 1# 3# s3 of
              s4 ->
                case writeArray# marr1 3# (U 11) s4 of
                  s5 ->
                    case freezeArray# marr2 0# 8# s5 of
                      (# s6, arr2 #) ->
                        case copyArray# arr2 1# marr2 1# 1# s6 of
                          s7 ->
                            case readArray# marr2 2# s7 of
                              (# s8, U val1 #) ->
                                case thawArray# arr2 1# 7# s8 of
                                  (# s9, marr2' #) ->
                                    case readArray# marr2' 5# s9 of
                                      (# s10, U val2 #) ->
                                        (# s10, [I# (sizeofMutableArray# marr2), val1, val2] #)
  print res
