-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.Trace
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Trace.hs,v 1.3 2002/04/24 16:31:43 simonmar Exp $
--
-- The trace function.
--
-----------------------------------------------------------------------------

module Debug.Trace (
	trace -- :: String -> a -> a
  ) where

import Prelude
import System.IO.Unsafe
import System.IO

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Handle
#endif

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE trace #-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    hPutStr stderr string
    hPutChar stderr '\n'
    fd <- withHandle_ "trace" stderr $ (return.haFD)
    postTraceHook fd
    return expr

foreign import ccall "PostTraceHook" postTraceHook :: Int -> IO ()
#endif
