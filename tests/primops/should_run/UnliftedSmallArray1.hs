{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  res <- IO \ s0 ->
      case newSmallArray# 4# (41 :: Int) s0 of
        (# s1, marr1 #) ->
          case newSmallArray# 7# (11 :: Int) s1 of
            (# s2, marr2 #) ->
              case unsafeFreezeSmallArray# marr1 s2 of
                (# s3, arr1 #) ->
                  case unsafeFreezeSmallArray# marr2 s3 of
                    (# s4, arr2 #) ->
                      case newSmallArray# 3# arr1 s4 of
                        (# s5, marrarr #) ->
                          case writeSmallArray# marrarr 2# arr2 s5 of
                            s6 ->
                              case unsafeFreezeSmallArray# marrarr s6 of
                                (# s7, arrarr #) ->
                                  case indexSmallArray# arrarr 2# of
                                    (# read_arr_2 #) ->
                                      case indexSmallArray# arrarr 0# of
                                        (# read_arr_0 #) ->
                                          case indexSmallArray# read_arr_2 6# of
                                            (# val_11 #) ->
                                              case indexSmallArray# read_arr_0 3#  of
                                                (# val_41 #) ->
                                                  (# s7, [I# (sizeofSmallArray# arrarr), val_11, val_41] #)
  print res
