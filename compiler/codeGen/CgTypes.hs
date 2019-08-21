{-# LANGUAGE CPP #-}

-- | Code generation related types used in other parts of the compiler.

-- In particular we define here any information which might be written to or
-- read from interface files.

module CgTypes
  ( CgIfaceInfo (..)
  ) where

#include "HsVersions.h"

import GhcPrelude

-----------------------------------------------------------------------------
--                General types
-----------------------------------------------------------------------------

-- | The information generated during code generation which will be written to
-- the interface file of the current module.
data CgIfaceInfo = CgIfaceInfo
