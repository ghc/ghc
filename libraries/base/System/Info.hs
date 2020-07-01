{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

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

import qualified Data.Text      as T
import           Data.Text.Read (decimal)
import           Data.Version
import           GHC.Version    (cProjectVersion)

-- | The short version of 'compilerName' with which the program was compiled
-- or is being interpreted. It only includes the major and minor version numbers.
compilerVersion :: Version
compilerVersion = Version [major, minor] []
  where (major, minor) = compilerVersionRaw `divMod` 100

-- | The full version of 'compilerName' with which the program was compiled
-- or is being interpreted. It includes the major, minor, revision and an additional
-- identifier, generally in the form "<year><month><day>".
fullCompilerVersion :: Version
fullCompilerVersion = Version version
  where
    version = parseVersion cProjectVersion

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
