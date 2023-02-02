{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Fingerprint (
        fingerprintString,
        fingerprintFingerprints
  ) where

import GHC.Base
import GHC.Fingerprint.Type

fingerprintFingerprints :: [Fingerprint] -> Fingerprint
fingerprintString :: String -> Fingerprint

