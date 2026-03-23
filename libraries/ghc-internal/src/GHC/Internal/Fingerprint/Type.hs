{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  GHC.Internal.Fingerprint.Type
-- Copyright   :  (c) The University of Glasgow, 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.

module GHC.Internal.Fingerprint.Type (Fingerprint(..)) where

import GHC.Internal.Base
import GHC.Internal.List (length, replicate)
import GHC.Internal.Num
import GHC.Internal.Show
import GHC.Internal.Word
import GHC.Internal.Numeric (showHex)

-- Using 128-bit MD5 fingerprints for now.

data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving ( Eq  -- ^ @since base-4.4.0.0
           , Ord -- ^ @since base-4.4.0.0
           )

-- | @since base-4.7.0.0
instance Show Fingerprint where
  show (Fingerprint w1 w2) = hex16 w1 ++ hex16 w2
    where
      -- Formats a 64 bit number as 16 digits hex.
      hex16 :: Word64 -> String
      hex16 i = let hex = showHex i ""
                 in replicate (16 - length hex) '0' ++ hex
