{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}

-- | @since 4.7.0.0
module GHC.Profiling
    ( stopProfTimer
    , startProfTimer
    -- * Root Profiling
    , Root(..)
    , setHeapProfilingRoots
    , maximumSupportedRoots
  ) where

#if defined(PROFILING)
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import GHC.List (length)
import GHC.Real (fromIntegral)
import Unsafe.Coerce
import GHC.Show (show)
#endif

import Data.IORef
import System.IO
import System.IO.Unsafe

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
  -- Note that this should be fairly short and not include any comma
  -- characters as closures reachable via multiple roots will be accounted
  -- into bands labeled with root names intercalated by commas.
  --
  -- For example if a closure is reachable via roots "r1", "r2" and "r3"
  -- the band in the graph would be called: @"r1,r2,r3"@.

  , rootClosure :: a
  -- ^ The Haskell value the root profiler starts traversing the heap
  -- from.
  --
  -- You should be mindful of the fact that data-type accessor functions
  -- will likely turn into thunks which reference the entire datastructure
  -- in the compiled program so prefer pattern matching to extract the
  -- value of interest.
  }

-- | This function informs the RTS of which closures the "root profiler"
-- should consider. See the Profiling chapter in the GHC User Guide,
-- section "Profiling by program-specified objects" for details.
--
-- Only a certain number of roots may be supported, see
-- 'maximumSupportedRoots'. If more than the supported number are passed
-- the list is truncated to the maximum length and a warning is printed.
--
-- When compiled without profiling this function will print a warning to
-- stderr, once. Use 'maximumSupportedRoots' to avoid printing the warning
-- if needed.
--
-- @since 4.14.0.0
setHeapProfilingRoots :: [Root] -> IO ()

-- | The maximum number of roots which can be set using
-- 'setHeapProfilingRoots'. 'Nothing' if program is not compiled with
-- profiling enabled.
--
-- @since 4.14.0.0
maximumSupportedRoots :: Maybe Int

emitHeapProfilingRootsWarning :: IORef Bool
emitHeapProfilingRootsWarning = unsafePerformIO (newIORef True)
{-# NOINLINE emitHeapProfilingRootsWarning #-}

#if defined(PROFILING)

foreign import ccall unsafe "setRootProfPtrs" c_setRootProfPtrs
  :: Int -> Ptr (StablePtr a) -> Ptr CString -> IO Int

setHeapProfilingRoots xs = do
  descs <- mapM (newCString . rootDescr) xs
  sps   <- mapM (\(Root _ a) -> newStablePtr (unsafeCoerce# a)) xs
  withArray descs $ \descs_arr ->
    withArray sps $ \sps_arr -> do
      let len = fromIntegral $ length xs
      len' <- c_setRootProfPtrs len sps_arr descs_arr
      when (len /= len') $ do
        x <- readIORef emitHeapProfilingRootsWarning
        when x $ do
          hPutStrLn stderr $
            "WARNING: setHeapProfilingRoots was called with "++show len++" roots, but only "++show len'++" are supported. The rest will be ignored."
          writeIORef emitHeapProfilingRootsWarning False

maximumSupportedRoots =
  Just $ unsafePerformIO $ c_setRootProfPtrs (-1) nullPtr nullPtr

#else

setHeapProfilingRoots _ = do
  x <- readIORef emitHeapProfilingRootsWarning
  when x $ do
    hPutStrLn stderr $
      "WARNING: setHeapProfilingRoots was called but this program is compiled without profiling enabled."
    writeIORef emitHeapProfilingRootsWarning False

maximumSupportedRoots = Nothing

#endif
