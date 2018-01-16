{-# LANGUAGE NoImplicitPrelude #-}

module C where

import A
import {-# SOURCE #-} B

mkTyConApp :: Fingerprint -> Fingerprint
mkTyConApp f = fingerprintFingerprints [f]
