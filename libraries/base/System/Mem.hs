-----------------------------------------------------------------------------
-- 
-- Module      :  System.Mem
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Mem.hs,v 1.2 2002/02/13 12:17:14 simonmar Exp $
--
-- Memory-related system things.
--
-----------------------------------------------------------------------------

module System.Mem (
 	performGC	-- :: IO ()
  ) where
 
import Prelude

#ifdef __GLASGOW_HASKELL__
foreign import {-safe-} "performGC" performGC :: IO ()
#endif
