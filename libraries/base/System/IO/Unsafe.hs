-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Unsafe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- "Unsafe" IO operations.
--
-----------------------------------------------------------------------------

module System.IO.Unsafe (
   unsafePerformIO,	-- :: IO a -> a
   unsafeInterleaveIO,	-- :: IO a -> IO a
  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif
