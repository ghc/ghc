{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  res <- IO \ s0 ->
      case newArray# 4# (41 :: Int) s0 of
        (# s1, marr1 #) ->
          case newArray# 7# (11 :: Int) s1 of
            (# s2, marr2 #) ->
              case unsafeFreezeArray# marr1 s2 of
                (# s3, arr1 #) ->
                  case unsafeFreezeArray# marr2 s3 of
                    (# s4, arr2 #) ->
                      case newArray# 3# arr1 s4 of
                        (# s5, marrarr #) ->
                          case writeArray# marrarr 2# arr2 s5 of
                            s6 ->
                              case unsafeFreezeArray# marrarr s6 of
                                (# s7, arrarr #) ->
                                  case indexArray# arrarr 2# of
                                    (# read_arr_2 #) ->
                                      case indexArray# arrarr 0# of
                                        (# read_arr_0 #) ->
                                          case indexArray# read_arr_2 6# of
                                            (# val_11 #) ->
                                              case indexArray# read_arr_0 3#  of
                                                (# val_41 #) ->
                                                  (# s7, [I# (sizeofArray# arrarr), val_11, val_41] #)
  print res
