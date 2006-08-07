-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.Trace
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'trace' function.
--
-----------------------------------------------------------------------------

module Debug.Trace (
	-- * Tracing
	putTraceMsg,      -- :: String -> IO ()
	trace             -- :: String -> a -> a
  ) where

import Prelude
import System.IO.Unsafe

#ifdef __GLASGOW_HASKELL__
import Foreign.C.String
#else
import System.IO (hPutStrLn,stderr)
#endif

-- | 'putTraceMsg' function outputs the trace message from IO monad.
-- Usually the output stream is 'System.IO.stderr' but if the function is called
-- from Windows GUI application then the output will be directed to the Windows
-- debug console.
putTraceMsg :: String -> IO ()
putTraceMsg msg = do
#ifndef __GLASGOW_HASKELL__
    hPutStrLn stderr msg
#else
    withCString "%s\n" $ \cfmt ->
     withCString msg  $ \cmsg ->
      debugBelch cfmt cmsg

foreign import ccall unsafe debugBelch :: CString -> CString -> IO ()
#endif

{-# NOINLINE trace #-}
{-|
When called, 'trace' outputs the string in its first argument, before 
returning the second argument as its result. The 'trace' function is not 
referentially transparent, and should only be used for debugging, or for 
monitoring execution. Some implementations of 'trace' may decorate the string 
that\'s output to indicate that you\'re tracing. The function is implemented on
top of 'putTraceMsg'.
-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    putTraceMsg string
    return expr

{-|
Like 'trace', but uses 'show' on the argument to convert it to a 'String'.

> traceShow = trace . show
-}
traceShow :: (Show a) => a -> b -> b
traceShow = trace . show
