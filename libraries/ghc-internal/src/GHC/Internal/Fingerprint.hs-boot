{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Fingerprint (
        fingerprintString,
        fingerprintFingerprints
  ) where

import GHC.Internal.Base
import GHC.Internal.Fingerprint.Type

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintString :: String -> Fingerprint

