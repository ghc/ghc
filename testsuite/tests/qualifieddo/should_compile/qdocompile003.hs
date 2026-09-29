{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- With -O, the block must compile to nested case expressions.
module QDoCompile003 where

import GHC.Exts
import qualified GHC.Internal.Unboxed.Do as Ubx

sumTwo :: MutableByteArray# s -> State# s -> (# State# s, Word# #)
sumTwo mba = Ubx.do
  a <- readWordArray# mba 0#
  b <- readWordArray# mba 1#
  writeWordArray# mba 2# (plusWord# a b)
  Ubx.pure (plusWord# a b)
