{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}

-- | @since 4.7.0.0
module GHC.Profiling where

#if defined(PROFILING)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import GHC.List (length)
import GHC.Real (fromIntegral)
import Unsafe.Coerce
#else
import System.IO
import System.IO.Unsafe
import Data.IORef
#endif

import GHC.Base

-- | Stop attributing ticks to cost centres. Allocations will still be
-- attributed.
--
-- @since 4.7.0.0
foreign import ccall stopProfTimer :: IO ()

-- | Start attributing ticks to cost centres. This is called by the RTS on
-- startup.
--
-- @since 4.7.0.0
foreign import ccall startProfTimer :: IO ()

-- | A container for a description and Haskell value for the puproses of
-- the root profiler. See 'setHeapProfilingRoots'
--
-- @since 4.14.0.0
data Root = forall a. Root
  { rootDescr   :: String
  -- ^ Description string used to identify this root in the heap profile
  -- graph when using @+RTS -ho@.
  --
  -- Note that this should be fairly short and not include the dash
  -- character (@'-'@) as closures reachable via multiple roots will be
  -- accounted into bands with names including all the relevant roots.
  --
  -- For example if a closure is reachable via roots "r1", "r2" and "r3"
  -- the band in the graph would be called: @"r1-r2-r3"@.

  , rootClosure :: a
  -- ^ The Haskell value the root profiler starts traversing the heap
  -- from.
  --
  -- Yoy should be mindful of the fact that data-type accessor functions
  -- will likely turn into thunks which reference the entire datastructure
  -- in the compiled program so prefer pattern matching to extract the
  -- value of interest.
  }

-- | This function informs the RTS of which closures the "root profiler"
-- should consider. See the Profiling chapter in the GHC User Guide,
-- section "Profiling by program-specified objects" for details.
--
-- When compiled without profiling this function will print a warning to
-- stderr, once. Use 'isCompiledWithProfiling' to avoid printing the
-- warning if needed.
--
-- @since 4.14.0.0
setHeapProfilingRoots :: [Root] -> IO ()

-- | Is this program being compiled with profiling?
--
-- @since 4.14.0.0
isCompiledWithProfiling :: Bool

#if defined(PROFILING)

foreign import ccall unsafe "setRootProfPtrs" c_setRootProfPtrs
  :: CInt -> Ptr (StablePtr a) -> Ptr CString -> IO ()

setHeapProfilingRoots xs = do
  descs <- mapM (newCString . rootDescr) xs
  sps   <- mapM (\(Root _ a) -> newStablePtr (unsafeCoerce# a)) xs
  withArray descs $ \descs_arr ->
    withArray sps $ \sps_arr ->
      c_setRootProfPtrs (fromIntegral (length xs)) sps_arr descs_arr

isCompiledWithProfiling = True

#else

setHeapProfilingRoots _ = do
  x <- readIORef emitHeapProfilingRootsWarning
  when x $ do
    hPutStrLn stderr $
      "WARNING: setHeapProfilingRoots was called but this program is compiled without profiling enabled."
    writeIORef emitHeapProfilingRootsWarning False

emitHeapProfilingRootsWarning :: IORef Bool
emitHeapProfilingRootsWarning = unsafePerformIO (newIORef True)
{-# NOINLINE emitHeapProfilingRootsWarning #-}

isCompiledWithProfiling = False

#endif
