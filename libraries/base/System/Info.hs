{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Info
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Information about the characteristics of the host
-- system lucky enough to run your program.
--
-- For a comprehensive listing of supported platforms, please refer to
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms
-----------------------------------------------------------------------------

module System.Info
  ( os
  , arch
  , compilerName
  , compilerVersion
  , fullCompilerVersion
  ) where

import           Data.Version (Version (..))

-- | The version of 'compilerName' with which the program was compiled
-- or is being interpreted.
--
-- ==== __Example__
-- > ghci> compilerVersion
-- > Version {versionBranch = [8,8], versionTags = []}
compilerVersion :: Version
compilerVersion = Version [major, minor] []
  where (major, minor) = compilerVersionRaw `divMod` 100

-- | The full version of 'compilerName' with which the program was compiled
-- or is being interpreted. It includes the major, minor, revision and an additional
-- identifier, generally in the form "<year><month><day>".
fullCompilerVersion :: Version
fullCompilerVersion = Version version []
  where
    version :: [Int]
    version = fmap read $ splitVersion __GLASGOW_HASKELL_FULL_VERSION__

splitVersion :: String -> [String]
splitVersion s =
  case dropWhile (== '.') s of
    "" -> []
    s' -> let (w, s'') = break (== '.') s'
           in w : splitVersion s''

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
