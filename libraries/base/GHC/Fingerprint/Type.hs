{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- ----------------------------------------------------------------------------
-- 
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning, and
-- implementing fast comparison of Typeable.
--
-- ----------------------------------------------------------------------------

module GHC.Fingerprint.Type (Fingerprint(..)) where

import GHC.Base
import GHC.Word

-- Using 128-bit MD5 fingerprints for now.

data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq, Ord)

