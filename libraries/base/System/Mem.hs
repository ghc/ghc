-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Memory-related system things.
--
-----------------------------------------------------------------------------

module System.Mem (
 	performGC	-- :: IO ()
  ) where
 
import Prelude

#ifdef __HUGS__
import Hugs.IOExts
#endif

#ifdef __GLASGOW_HASKELL__
-- | Triggers an immediate garbage collection
foreign import ccall {-safe-} "performMajorGC" performGC :: IO ()
#endif

#ifdef __NHC__
import NHC.IOExtras (performGC)
#endif
