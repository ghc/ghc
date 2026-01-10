{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.ByteOrder
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Host byte ordering.
--
-- @since base-4.11.0.0
-----------------------------------------------------------------------------

module GHC.Internal.ByteOrder
  ( ByteOrder(..)
  , hostByteOrder
  ) where

-- Required for WORDS_BIGENDIAN
#include <ghcautoconf.h>

import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Text.Show

-- | Byte ordering.
data ByteOrder
    = BigEndian    -- ^ most-significant-byte occurs in lowest address.
    | LittleEndian -- ^ least-significant-byte occurs in lowest address.
    deriving ( Eq      -- ^ @since base-4.11.0.0
             , Ord     -- ^ @since base-4.11.0.0
             , Bounded -- ^ @since base-4.11.0.0
             , Enum    -- ^ @since base-4.11.0.0
             , Show    -- ^ @since base-4.11.0.0
             )

-- | The byte ordering of the host machine, i.e. the machine on which
-- the code examining the @hostByteOrder@ predicate is being executed.
--
-- @since base-4.23.0.0
hostByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
hostByteOrder = BigEndian
#else
hostByteOrder = LittleEndian
#endif
