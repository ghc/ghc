<-----------------------------------------------------------------------------
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
       compilerVersion	    -- :: Version
   ) where

import Prelude
import Data.Version

compilerVersion :: Version
compilerVersion = Version {versionBranch=[maj,min], versionTags=[]}
  where (maj,min) = compilerVersionRaw `divMod` 100

os, arch, compilerName :: String
compilerVersionRaw :: Int

#if defined(__NHC__)
#include "OSInfo.hs"
compilerName = "nhc98"
compilerVersionRaw = __NHC__

#elif defined(__GLASGOW_HASKELL__)
#include "ghcplatform.h"
os = HOST_OS
arch = HOST_ARCH
compilerName = "ghc"
compilerVersionRaw = __GLASGOW_HASKELL__

#elif defined(__HUGS__)
#include "platform.h"
os = HOST_OS
arch = HOST_ARCH
compilerName = "hugs"
compilerVersionRaw = 0  -- ToDo

#else
#error Unknown compiler name
#endif
