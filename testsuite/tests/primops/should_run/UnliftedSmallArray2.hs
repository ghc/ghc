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
    case newSmallArray# 7# (U 3) s1 of
      (# s2, marr1 #) ->
        case newSmallArray# 9# (U 8) s2 of
          (# s3, marr2 #) ->
            case copySmallMutableArray# marr1 2# marr2 1# 3# s3 of
              s4 ->
                case writeSmallArray# marr1 3# (U 11) s4 of
                  s5 ->
                    case freezeSmallArray# marr2 0# 8# s5 of
                      (# s6, arr2 #) ->
                        case copySmallArray# arr2 1# marr2 1# 1# s6 of
                          s7 ->
                            case readSmallArray# marr2 2# s7 of
                              (# s8, U val1 #) ->
                                case thawSmallArray# arr2 1# 7# s8 of
                                  (# s9, marr2' #) ->
                                    case shrinkSmallMutableArray# marr2' 6# s9 of
                                      s10 ->
                                        case readSmallArray# marr2' 5# s10 of
                                          (# s11, U val2 #) ->
                                            case getSizeofSmallMutableArray# marr2' s11 of
                                              (# s12, sz #) ->
                                                (# s12, [I# sz, val1, val2] #)
  print res
