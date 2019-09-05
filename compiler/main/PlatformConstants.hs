{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
--
-- | Platform constants
--
-- (c) The University of Glasgow 2013
--
-------------------------------------------------------------------------------

module PlatformConstants
  ( PlatformConstants(..)
  , HasPlatformConstants(..)
#include "GHCConstantsHaskellExports.hs"
  ) where

import GhcPrelude

-- Produced by deriveConstants
#include "GHCConstantsHaskellType.hs"

-- Produced by deriveConstants
#include "GHCConstantsHaskellWrappers.hs"
