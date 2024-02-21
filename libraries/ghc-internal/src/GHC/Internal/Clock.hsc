{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Clock
    ( getMonotonicTime
    , getMonotonicTimeNSec
    ) where

import GHC.Internal.Base
import GHC.Internal.Real
import GHC.Internal.Word
import GHC.Internal.Float () -- for Num Double instance
#if defined(javascript_HOST_ARCH)
import GHC.Internal.Num
#endif

-- | Return monotonic time in seconds, since some unspecified starting point
--
-- @since base-4.11.0.0
getMonotonicTime :: IO Double
getMonotonicTime = do
#if defined(javascript_HOST_ARCH)
  w <- getMonotonicTimeMSec
  return (w / 1000)
#else
  w <- getMonotonicTimeNSec
  return (fromIntegral w / 1000000000)
#endif

-- | Return monotonic time in nanoseconds, since some unspecified starting point
--
-- @since base-4.11.0.0
#if defined(javascript_HOST_ARCH)
getMonotonicTimeNSec :: IO Word64
getMonotonicTimeNSec = do
  w <- getMonotonicTimeMSec
  return (floor w * 1000000)

foreign import javascript unsafe "performance.now" getMonotonicTimeMSec:: IO Double


#else
foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicTimeNSec :: IO Word64
#endif
