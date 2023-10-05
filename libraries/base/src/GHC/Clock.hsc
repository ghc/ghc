{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Clock
    ( getMonotonicTime
    , getMonotonicTimeNSec
    ) where

import GHC.Base
import GHC.Real
import Data.Word
#if defined(javascript_HOST_ARCH)
import GHC.Num
#endif

-- | Return monotonic time in seconds, since some unspecified starting point
--
-- @since 4.11.0.0
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
-- @since 4.11.0.0
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
