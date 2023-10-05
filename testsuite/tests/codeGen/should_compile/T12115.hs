{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T12115 where

import GHC.Exts
import GHC.Types

f :: (# (# #), (# #) #) -> String
f = f
