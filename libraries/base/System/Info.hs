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

#ifndef __NHC__
#include "MachDeps.h"
#endif

module System.Info
   (
       os,		    -- :: String
       arch		    -- :: String
   ) where

import Prelude

#ifndef __NHC__

arch :: String
arch = HOST_ARCH

os :: String
os = HOST_OS

#else
os,arch ::String
#include "OSInfo.hs"
#endif
