{-# OPTIONS_GHC -O1 #-}
{-# LANGUAGE MagicHash #-}
module T22282A where

import Data.Word
import GHC.Prim
import GHC.Word

wtestF :: GHC.Prim.Word8# -> GHC.Prim.Word8# -> GHC.Prim.Word8#
wtestF a b = case word8ToWord# b of
  0## -> a
  _   -> plusWord8# (timesWord8# (quotWord8# a b) b) (remWord8# a b)
{-# NOINLINE wtestF #-}

testF :: Word8 -> Word8 -> Word8
testF (W8# a) (W8# b) = W8# (wtestF a b)
{-# INLINE testF #-}

