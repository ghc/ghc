{-# LANGUAGE Safe #-}

module GHC.Fingerprint (
        Fingerprint(..), fingerprint0,
        fingerprintData,
        fingerprintString,
        fingerprintFingerprints,
        getFileHash, 
        getDirHash,
   ) where

import GHC.Internal.Fingerprint
