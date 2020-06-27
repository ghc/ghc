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
-- For a comprehensive listing of supported platforms, please refer to
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms
-----------------------------------------------------------------------------

module System.Info
   (
       os,
       arch,
       compilerName,
       compilerVersion
   ) where

import Data.Version

-- | The version of 'compilerName' with which the program was compiled
-- or is being interpreted.
-- 
-- ==== __Example__
-- > ghci> compilerVersion
-- > Version {versionBranch = [8,8], versionTags = []}
compilerVersion :: Version
compilerVersion = Version [major, minor] []
  where (major, minor) = compilerVersionRaw `divMod` 100

#include "ghcplatform.h"

-- | The operating system on which the program is running.
-- Common values include:
--
--     * "darwin" — macOS
--     * "freebsd"
--     * "linux"
--     * "linux-android"
--     * "mingw32" — Windows
--     * "netbsd"
--     * "openbsd"
os :: String
os = HOST_OS

-- | The machine architecture on which the program is running.
-- Common values include:
--
--    * "aarch64"
--    * "alpha"
--    * "arm"
--    * "hppa"
--    * "hppa1_1"
--    * "i386"
--    * "ia64"
--    * "m68k"
--    * "mips"
--    * "mipseb"
--    * "mipsel"
--    * "nios2"
--    * "powerpc"
--    * "powerpc64"
--    * "powerpc64le"
--    * "riscv32"
--    * "riscv64"
--    * "rs6000"
--    * "s390"
--    * "s390x"
--    * "sh4"
--    * "sparc"
--    * "sparc64"
--    * "vax"
--    * "x86_64"
arch :: String
arch = HOST_ARCH

-- | The Haskell implementation with which the program was compiled
-- or is being interpreted.
-- On the GHC platform, the value is "ghc".
compilerName :: String
compilerName = "ghc"

compilerVersionRaw :: Int
compilerVersionRaw = __GLASGOW_HASKELL__
