-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- ----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fingerprint (
        Fingerprint(..), fingerprint0,
        readHexFingerprint,
        fingerprintData,
        fingerprintString
   ) where

#include "md5.h"
##include "HsVersions.h"

import Outputable

import Text.Printf
import Numeric          ( readHex )

import GHC.Fingerprint

instance Outputable Fingerprint where
  ppr (Fingerprint w1 w2) = text (printf "%016x%016x" w1 w2)

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)

