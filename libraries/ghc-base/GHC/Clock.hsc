{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Clock
    ( getMonotonicTime
    , getMonotonicTimeNSec
    ) where

import GHC.Base
import GHC.Real
import Data.Word

-- | Return monotonic time in seconds, since some unspecified starting point
--
-- @since 4.11.0.0
getMonotonicTime :: IO Double
getMonotonicTime = do w <- getMonotonicTimeNSec
                      return (fromIntegral w / 1000000000)

-- | Return monotonic time in nanoseconds, since some unspecified starting point
--
-- @since 4.11.0.0
foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicTimeNSec :: IO Word64

