{-# LANGUAGE CPP #-}

-- | Code generation related types used in other parts of the compiler.

-- In particular we define here any information which might be written to or
-- read from interface files.

module CgTypes
  ( CgIfaceInfo (..)
  , PackageCgInfo (..)
  , emptyPackageCgInfo
  ) where

#include "HsVersions.h"

import GhcPrelude

import Binary

--------------------------------------------------------------------------------

-- | The information generated during code generation which will be written to
-- the interface file of the current module.
data CgIfaceInfo = CgIfaceInfo

-- | Codegen info for imported packages. Part of ExternalPackageState.
data PackageCgInfo = PackageCgInfo

emptyPackageCgInfo :: PackageCgInfo
emptyPackageCgInfo = PackageCgInfo

instance Binary CgIfaceInfo where
  put_ _bh CgIfaceInfo = return ()
  get _bh = return CgIfaceInfo
