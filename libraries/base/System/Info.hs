-----------------------------------------------------------------------------
-- |
-- Module      :  System.Info
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Info.hs,v 1.3 2002/04/24 16:31:45 simonmar Exp $
--
-- Misc information about the characteristics of the host 
-- architecture/machine lucky enough to run your program.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module System.Info
   (
       os,		    -- :: String
       arch		    -- :: String
   ) where

import Prelude

arch :: String
arch = HOST_ARCH

os :: String
os = HOST_OS
