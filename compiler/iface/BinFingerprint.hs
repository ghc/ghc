{-# LANGUAGE CPP #-}

-- | Computing fingerprints of values serializeable with GHC's "Binary" module.
module BinFingerprint
  ( -- * Computing fingerprints
    fingerprintBinMem
  , computeFingerprint
  , putNameLiterally
  ) where

#include "HsVersions.h"

import GhcPrelude

import Fingerprint
import Binary
import Binary.Unsafe (runBuffer)
import Name
import PlainPanic
import Util

fingerprintBinMem :: BinData -> IO Fingerprint
fingerprintBinMem bh = withBinBuffer bh f
  where
    f bs =
        -- we need to take care that we force the result here
        -- lest a reference to the ByteString may leak out of
        -- withBinBuffer.
        let fp = fingerprintByteString bs
        in fp `seq` return fp

computeFingerprint :: (Binary a)
                   => (Name -> Put ())
                   -> a
                   -> IO Fingerprint
computeFingerprint put_nonbinding_name a = do
  bd <- runBuffer (3 * 1024) (setUserData (put a)) -- just less than a block
  fingerprintBinMem bd
  where
    setUserData =
      writeState put_nonbinding_name putNameLiterally putFS

-- | Used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: Name -> Put ()
putNameLiterally name = ASSERT( isExternalName name ) do
    put $! nameModule name
    put $! nameOccName name
