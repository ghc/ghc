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
-- Misc information about the characteristics of the host 
-- architecture\/machine lucky enough to run your program.
--
-----------------------------------------------------------------------------

module System.Info
   (
       os,		    -- :: String
       arch,		    -- :: String
       compilerName,	    -- :: String
#ifdef __GLASGOW_HASKELL__
       compilerVersion	    -- :: Version
#endif
   ) where

import Prelude
import Data.Version

#ifndef __NHC__

#include "ghcplatform.h"

arch :: String
arch = HOST_ARCH

os :: String
os = HOST_OS

#else
os,arch ::String
#include "OSInfo.hs"
#endif

compilerName :: String
#if defined(__NHC__)
compilerName = "nhc98"
#elif defined(__GLASGOW_HASKELL__)
compilerName = "ghc"
#elif defined(__HUGS__)
compilerName = "hugs"
#else
#error Unknown compiler name
#endif

#ifdef __GLASGOW_HASKELL__
compilerVersion :: Version
compilerVersion = Version {versionBranch=[maj,min], versionTags=[]}
  where (maj,min) = __GLASGOW_HASKELL__ `divMod` 100
#endif

#ifdef __NHC__
compilerVersion :: Version
compilerVersion = Version {versionBranch=[maj,min], versionTags=[]}
  where version = __NHC__ `divMod` 100
        maj = fst version
        min = snd version
#endif

