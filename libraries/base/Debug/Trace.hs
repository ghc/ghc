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

#ifdef __GLASGOW_HASKELL__
#include "ghcconfig.h"
#endif

module Debug.Trace (
	-- * Tracing
	
	-- ** Tracers
	-- | The tracer is a function that monitors the trace messages.
	fileTracer,       -- :: Handle -> String -> IO ()
#ifdef mingw32_HOST_OS
	winDebugTracer,   -- :: String -> IO ()
#endif
	addTracer,        -- :: String -> (String -> IO ()) -> IO ()
	removeTracer,     -- :: String -> IO ()
	
	-- ** Messages
	putTraceMsg,      -- :: String -> IO ()
	trace             -- :: String -> a -> a
  ) where

import Prelude
import Data.IORef
import System.IO.Unsafe
import System.IO

#ifdef mingw32_HOST_OS
import Foreign.C.String
#endif

{-# NOINLINE tracers #-}
tracers :: IORef [(String, String -> IO ())]
tracers = unsafePerformIO (newIORef [("defaultTracer", fileTracer stderr)])

-- | A tracer function that outputs the message to a file
fileTracer :: Handle     -- ^ file handle
           -> String     -- ^ trace message
           -> IO ()
fileTracer handle msg = do
   hPutStr handle msg
   hPutChar handle '\n'

#ifdef mingw32_HOST_OS
-- | A tracer function that outputs the message to the debuger (Windows only)
winDebugTracer :: String  -- ^ trace message
               -> IO ()
winDebugTracer msg = do
   withCString (msg++"\n") outputDebugString

foreign import ccall unsafe "OutputDebugStringA"
  outputDebugString :: CString -> IO ()
#endif

-- | Registering a new tracer
addTracer :: String             -- ^ the tracer name
          -> (String -> IO ())  -- ^ tracer
          -> IO ()
addTracer name tracer = do
	ts <- readIORef tracers
	writeIORef tracers ((name,tracer):filter (\(n,l) -> n /= name) ts)

-- | Removing the tracer with the given name
removeTracer :: String -> IO ()
removeTracer name = do
	ts <- readIORef tracers
	writeIORef tracers (filter (\(n,l) -> n /= name) ts)

-- | 'putTraceMsg' function outputs the trace message from IO monad.
putTraceMsg :: String -> IO ()
putTraceMsg msg = do
	ts <- readIORef tracers
	mapM_ (\(n,l) -> l msg) ts

{-# NOINLINE trace #-}
{-|
When called, 'trace' outputs the string in its first argument using the
installed tracers, before returning the second argument as its result.
The 'trace' function is not referentially transparent, and should only
be used for debugging, or for monitoring execution. Some
implementations of 'trace' may decorate the string that\'s output to
indicate that you\'re tracing.
-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    putTraceMsg string
    return expr
