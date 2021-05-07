
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance
--
-- ----------------------------------------------------------------------------

module GHC.Utils.Fingerprint (
        readHexFingerprint,
        fingerprintByteString,
        -- * Re-exported from GHC.Fingerprint
        Fingerprint(..), fingerprint0,
        fingerprintFingerprints,
        fingerprintData,
        fingerprintString,
        getFileHash
   ) where

import GHC.Prelude

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

fingerprintByteString :: BS.ByteString -> Fingerprint
fingerprintByteString bs = unsafeDupablePerformIO $
  BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> fingerprintData (castPtr ptr) len
