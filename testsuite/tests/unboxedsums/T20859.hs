{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}

module T20859 where

import GHC.Exts
  ( Double#, Int#, Word# )

foo :: (# Int# | Double# | Word# #) -> (# | | #) Int# Double# Word#
foo x = x
