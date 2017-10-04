{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ByteOrder
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Target byte ordering.
--
-----------------------------------------------------------------------------

module GHC.ByteOrder where

-- | Byte ordering.
data ByteOrder
    = BigEndian    -- ^ most-significant-byte occurs in lowest address.
    | LittleEndian -- ^ least-significant-byte occurs in lowest address.
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

-- | The byte ordering of the target machine.
targetByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
targetByteOrder = BigEndian
#else
targetByteOrder = LittleEndian
#endif
