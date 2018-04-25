{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T12115 where

import GHC.Prim
import GHC.Types

f :: (# Void#, (# #) #) -> String
f = f
