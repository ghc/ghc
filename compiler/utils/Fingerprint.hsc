{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- ----------------------------------------------------------------------------

module Fingerprint (
        readHexFingerprint,
        fingerprintByteString,
        -- * Re-exported from GHC.Fingerprint
        Fingerprint(..), fingerprint0,
        fingerprintFingerprints,
        fingerprintData,
        fingerprintString,
        getFileHash
   ) where

#include "md5.h"
##include "HsVersions.h"

import Foreign
import GHC.IO
import Numeric          ( readHex )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import GHC.Fingerprint

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)

-- this can move to GHC.Fingerprint in GHC 8.6
fingerprintByteString :: BS.ByteString -> Fingerprint
fingerprintByteString bs = unsafeDupablePerformIO $
  BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> fingerprintData (castPtr ptr) len
