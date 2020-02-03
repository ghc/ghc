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
import Name
import PlainPanic
import Util

fingerprintBinMem :: BinHandle -> IO Fingerprint
fingerprintBinMem bh = withBinBuffer bh f
  where
    f bs =
        -- we need to take care that we force the result here
        -- lest a reference to the ByteString may leak out of
        -- withBinBuffer.
        let fp = fingerprintByteString bs
        in fp `seq` return fp

computeFingerprint :: (Binary a)
                   => (BinHandle -> Name -> IO ())
                   -> a
                   -> IO Fingerprint
computeFingerprint put_nonbinding_name a = do
  bh <- openBinMem (3*1024)
  writeState bh put_nonbinding_name putNameLiterally putFS $ \bh -> do
    put_ bh a
    fp <- fingerprintBinMem bh
    return fp

-- | Used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = ASSERT( isExternalName name ) do
    put_ bh $! nameModule name
    put_ bh $! nameOccName name
