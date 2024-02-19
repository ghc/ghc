{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Data.Bits
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines bitwise operations for signed and unsigned
-- integers.  Instances of the class 'Bits' for the 'Int' and
-- 'Integer' types are available from this module, and instances for
-- explicitly sized integral types are available from the
-- "Data.Int" and "Data.Word" modules.
--

module Data.Bits
    (-- *  Type classes
     Bits((.&.), (.|.), xor, complement, shift, rotate, zeroBits, bit, setBit, clearBit, complementBit, testBit, bitSizeMaybe, bitSize, isSigned, shiftL, shiftR, unsafeShiftL, unsafeShiftR, rotateL, rotateR, popCount),
     FiniteBits(finiteBitSize, countLeadingZeros, countTrailingZeros),
     -- *  Extra functions
     bitDefault,
     testBitDefault,
     popCountDefault,
     toIntegralSized,
     oneBits,
     (.^.),
     (.>>.),
     (.<<.),
     (!>>.),
     (!<<.),
     -- *  Newtypes
     And(..),
     Ior(..),
     Xor(..),
     Iff(..)
     ) where

import GHC.Internal.Data.Bits