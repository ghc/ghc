{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Bits
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Bits.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Bitwise operations.
--
-----------------------------------------------------------------------------

module Data.Bits ( 
  Bits(
    (.&.), (.|.), xor, -- :: a -> a -> a
    complement,        -- :: a -> a
    shift,             -- :: a -> Int -> a
    rotate,            -- :: a -> Int -> a
    bit,               -- :: Int -> a
    setBit,            -- :: a -> Int -> a
    clearBit,          -- :: a -> Int -> a
    complementBit,     -- :: a -> Int -> a
    testBit,           -- :: a -> Int -> Bool
    bitSize,           -- :: a -> Int
    isSigned           -- :: a -> Bool
  ),
  shiftL, shiftR,      -- :: Bits a => a -> Int -> a
  rotateL, rotateR,    -- :: Bits a => a -> Int -> a
  -- instance Bits Int
  -- instance Bits Integer
 ) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

#ifdef __GLASGOW_HASKELL__
#include "MachDeps.h"
import GHC.Num
import GHC.Real
import GHC.Base
#endif

--ADR: The fixity for .|. conflicts with that for .|. in Fran.
--     Removing all fixities is a fairly safe fix; fixing the "one fixity
--     per symbol per program" limitation in Hugs would take a lot longer.
#ifndef __HUGS__
infixl 8 `shift`, `rotate`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.
#endif

class Num a => Bits a where
    (.&.), (.|.), xor :: a -> a -> a
    complement        :: a -> a
    shift             :: a -> Int -> a
    rotate            :: a -> Int -> a
    bit               :: Int -> a
    setBit            :: a -> Int -> a
    clearBit          :: a -> Int -> a
    complementBit     :: a -> Int -> a
    testBit           :: a -> Int -> Bool
    bitSize           :: a -> Int
    isSigned          :: a -> Bool

    bit i               = 1 `shift` i
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i
    x `testBit` i       = (x .&. bit i) /= 0

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
x `shiftL`  i = x `shift`  i
x `shiftR`  i = x `shift`  (-i)
x `rotateL` i = x `rotate` i
x `rotateR` i = x `rotate` (-i)

#ifdef __GLASGOW_HASKELL__
instance Bits Int where
    (I# x#) .&.   (I# y#)  = I# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I# x#) .|.   (I# y#)  = I# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I# x#) `xor` (I# y#)  = I# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I# x#)     = I# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I# x#) `shift` (I# i#)
        | i# >=# 0#            = I# (x# `iShiftL#` i#)
        | otherwise            = I# (x# `iShiftRA#` negateInt# i#)
    (I# x#) `rotate` (I# i#) =
#if WORD_SIZE_IN_BYTES == 4
        I# (word2Int# ((x'# `shiftL#` i'#) `or#`
                       (x'# `shiftRL#` (32# -# i'#))))
        where
        x'# = int2Word# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# 31#)
#else
        I# (word2Int# ((x'# `shiftL#` i'#) `or#`
                       (x'# `shiftRL#` (64# -# i'#))))
        where
        x'# = int2Word# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# 63#)
#endif
    bitSize  _                 = WORD_SIZE_IN_BYTES * 8
    isSigned _                 = True

instance Bits Integer where
   (S# x) .&. (S# y) = S# (word2Int# (int2Word# x `and#` int2Word# y))
   x@(S# _) .&. y = toBig x .&. y
   x .&. y@(S# _) = x .&. toBig y
   (J# s1 d1) .&. (J# s2 d2) = 
	case andInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   (S# x) .|. (S# y) = S# (word2Int# (int2Word# x `or#` int2Word# y))
   x@(S# _) .|. y = toBig x .|. y
   x .|. y@(S# _) = x .|. toBig y
   (J# s1 d1) .|. (J# s2 d2) = 
	case orInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   (S# x) `xor` (S# y) = S# (word2Int# (int2Word# x `xor#` int2Word# y))
   x@(S# _) `xor` y = toBig x `xor` y
   x `xor` y@(S# _) = x `xor` toBig y
   (J# s1 d1) `xor` (J# s2 d2) =
	case xorInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   complement (S# x) = S# (word2Int# (int2Word# x `xor#` int2Word# (0# -# 1#)))
   complement (J# s d) = case complementInteger# s d of (# s, d #) -> J# s d

   shift x i | i >= 0    = x * 2^i
	     | otherwise = x `div` 2^(-i)

   rotate x i = shift x i   -- since an Integer never wraps around

   bitSize _  = error "Bits.bitSize(Integer)"
   isSigned _ = True
#endif
