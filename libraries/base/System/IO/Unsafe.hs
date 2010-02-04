{-# OPTIONS_GHC -XNoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Unsafe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- \"Unsafe\" IO operations.
--
-----------------------------------------------------------------------------

module System.IO.Unsafe (
   -- * Unsafe 'System.IO.IO' operations
   unsafePerformIO,     -- :: IO a -> a
   unsafeInterleaveIO,  -- :: IO a -> IO a
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.IO (unsafePerformIO, unsafeInterleaveIO)
#endif

#ifdef __HUGS__
import Hugs.IOExts (unsafePerformIO, unsafeInterleaveIO)
#endif

#ifdef __NHC__
import NHC.Internal (unsafePerformIO, unsafeInterleaveIO)
#endif

