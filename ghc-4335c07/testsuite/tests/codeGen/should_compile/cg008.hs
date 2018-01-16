{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -O0 #-}

-- Variant of cgrun066; compilation as a module is different.

module Cg008 (hashStr) where

import Foreign.C
import Data.Word
import Foreign.Ptr
import GHC.Exts

import Control.Exception

hashStr  :: Ptr Word8 -> Int -> Int
hashStr (Ptr a#) (I# len#) = loop 0# 0#
   where
    loop h n | isTrue# (n GHC.Exts.==# len#) = I# h
             | otherwise  = loop h2 (n GHC.Exts.+# 1#)
          where !c = ord# (indexCharOffAddr# a# n)
                !h2 = (c GHC.Exts.+# (h GHC.Exts.*# 128#)) `remInt#` 4091#
