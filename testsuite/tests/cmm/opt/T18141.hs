{-# LANGUAGE MagicHash #-}

module T18141 where

import GHC.Exts

divInt8# :: Int8# -> Int8# -> Int8#
x# `divInt8#` y#
  | isTrue# (x# `gtInt8#` zero#) && isTrue# (y# `ltInt8#` zero#) =
    ((x# `subInt8#` one#) `quotInt8#` y#) `subInt8#` one#
  | isTrue# (x# `ltInt8#` zero#) && isTrue# (y# `gtInt8#` zero#) =
    ((x# `plusInt8#` one#) `quotInt8#` y#) `subInt8#` one#
  | otherwise = x# `quotInt8#` y#
  where
    zero# = intToInt8# 0#
    one# = intToInt8# 1#

