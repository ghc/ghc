{-# LANGUAGE UnboxedTuples, MagicHash #-}

module T20214 where

import GHC.Exts

main = print $ case unpackDoubleX2# (broadcastDoubleX2# 4.4##) of
  (# a, b #) -> (D# a, D# b)
