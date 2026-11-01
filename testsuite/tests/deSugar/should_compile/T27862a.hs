{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Woverflowed-literals #-}

-- We expect only /one/ warning

module T27862a where

import Data.Word

f :: Word8 -> Int
f !x | even x = 0
f 300 = 1
f _   = 2
