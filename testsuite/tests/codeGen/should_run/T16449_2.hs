{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Prim
import GHC.Int

main = print (I# (uncheckedIShiftL# 1# 9223372036854775807#))
