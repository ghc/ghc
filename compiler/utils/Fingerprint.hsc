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
        Fingerprint(..), fingerprint0,
        readHexFingerprint,
        fingerprintData,
        fingerprintString,
        -- Re-exported from GHC.Fingerprint for GHC >= 7.7, local otherwise
        getFileHash
   ) where

#include "md5.h"
##include "HsVersions.h"

import Numeric          ( readHex )
#if __GLASGOW_HASKELL__ < 707
-- Only needed for getFileHash below.
import Foreign
import Panic
import System.IO
import Control.Monad    ( when )
#endif

import GHC.Fingerprint

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)


#if __GLASGOW_HASKELL__ < 707
-- Only use this if we're smaller than GHC 7.7, otherwise
-- GHC.Fingerprint exports a better version of this function.

-- | Computes the hash of a given file.
-- It loads the full file into memory an does not work with files bigger than
-- MAXINT.
getFileHash :: FilePath -> IO Fingerprint
getFileHash path = withBinaryFile path ReadMode $ \h -> do

  fileSize <- toIntFileSize `fmap` hFileSize h

  allocaBytes fileSize $ \bufPtr -> do
    n <- hGetBuf h bufPtr fileSize
    when (n /= fileSize) readFailedError
    fingerprintData bufPtr fileSize

  where
    toIntFileSize :: Integer -> Int
    toIntFileSize size
      | size > fromIntegral (maxBound :: Int) = throwGhcException $
          Sorry $ "Fingerprint.getFileHash: Tried to calculate hash of file "
                  ++ path ++ " with size > maxBound :: Int. This is not supported."
      | otherwise = fromIntegral size

    readFailedError = throwGhcException $
        Panic $ "Fingerprint.getFileHash: hGetBuf failed on interface file"
#endif
