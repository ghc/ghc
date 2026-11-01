{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Woverflowed-literals #-}

module T27862b where

import Data.Word

f :: Word8 -> Int
f !300 = 1
f _   = 2
