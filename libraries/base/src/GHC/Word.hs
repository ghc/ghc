{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Word
-- Copyright   :  (c) The University of Glasgow, 1997-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Sized unsigned integral types: 'Word', 'Word8', 'Word16', 'Word32', and
-- 'Word64'.
--

module GHC.Word
    (Word(..),
     Word8(..),
     Word16(..),
     Word32(..),
     Word64(..),
     -- *  Shifts
     uncheckedShiftL64#,
     uncheckedShiftRL64#,
     -- *  Byte swapping
     byteSwap16,
     byteSwap32,
     byteSwap64,
     -- *  Bit reversal
     bitReverse8,
     bitReverse16,
     bitReverse32,
     bitReverse64,
     -- *  Equality operators
     -- |  See GHC.Classes#matching_overloaded_methods_in_rules
     eqWord,
     neWord,
     gtWord,
     geWord,
     ltWord,
     leWord,
     eqWord8,
     neWord8,
     gtWord8,
     geWord8,
     ltWord8,
     leWord8,
     eqWord16,
     neWord16,
     gtWord16,
     geWord16,
     ltWord16,
     leWord16,
     eqWord32,
     neWord32,
     gtWord32,
     geWord32,
     ltWord32,
     leWord32,
     eqWord64,
     neWord64,
     gtWord64,
     geWord64,
     ltWord64,
     leWord64
     ) where

import GHC.Internal.Word
