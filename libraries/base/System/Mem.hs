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

#ifdef __GLASGOW_HASKELL__
foreign import ccall {-safe-} "performGC" performGC :: IO ()
#endif
