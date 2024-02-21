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
-- Target byte ordering.
--
-- @since base-4.11.0.0
-----------------------------------------------------------------------------

module GHC.Internal.ByteOrder
  ( ByteOrder(..)
  , targetByteOrder
  ) where

-- Required for WORDS_BIGENDIAN
#include <ghcautoconf.h>

import GHC.Internal.Base
import GHC.Internal.Enum
import GHC.Internal.Generics (Generic)
import GHC.Internal.Text.Read
import GHC.Internal.Text.Show

-- | Byte ordering.
data ByteOrder
    = BigEndian    -- ^ most-significant-byte occurs in lowest address.
    | LittleEndian -- ^ least-significant-byte occurs in lowest address.
    deriving ( Eq      -- ^ @since base-4.11.0.0
             , Ord     -- ^ @since base-4.11.0.0
             , Bounded -- ^ @since base-4.11.0.0
             , Enum    -- ^ @since base-4.11.0.0
             , Read    -- ^ @since base-4.11.0.0
             , Show    -- ^ @since base-4.11.0.0
             , Generic -- ^ @since base-4.15.0.0
             )

-- | The byte ordering of the target machine.
targetByteOrder :: ByteOrder
#if defined(WORDS_BIGENDIAN)
targetByteOrder = BigEndian
#else
targetByteOrder = LittleEndian
#endif
