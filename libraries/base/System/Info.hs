{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Info
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Information about the characteristics of the host 
-- system lucky enough to run your program.
--
-----------------------------------------------------------------------------

module System.Info
   (
       os,
       arch,
       compilerName,
       compilerVersion
   ) where

import Prelude
import Data.Version

-- | The version of 'compilerName' with which the program was compiled
-- or is being interpreted.
compilerVersion :: Version
compilerVersion = Version {versionBranch=[major, minor], versionTags=[]}
  where (major, minor) = compilerVersionRaw `divMod` 100

#include "ghcplatform.h"

-- | The operating system on which the program is running.
os :: String
os = HOST_OS

-- | The machine architecture on which the program is running.
arch :: String
arch = HOST_ARCH

-- | The Haskell implementation with which the program was compiled
-- or is being interpreted.
compilerName :: String
compilerName = "ghc"

compilerVersionRaw :: Int
compilerVersionRaw = __GLASGOW_HASKELL__
