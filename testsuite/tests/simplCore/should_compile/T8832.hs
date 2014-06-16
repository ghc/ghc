{-# LANGUAGE CPP #-}

-- I'm concerned that the -ddump-simpl output may differ on 32 and 64-bit
-- platforms.  So far I've only put in output for 64-bit platforms.

module T8832 where

import Data.Bits
import Data.Int
import Data.Word

#define T(s,T) \
s :: T ; \
s = clearBit (bit 0) 0 ; \

T(i,Int)
T(i8,Int8)
T(i16,Int16)
T(i32,Int32)
T(i64,Int64)

T(w,Word)
T(w8,Word8)
T(w16,Word16)
T(w32,Word32)
T(w64,Word64)

T(z,Integer)