{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Event.Clock
    ( getMonotonicTime
    , getMonotonicTimeNSec
    ) where

import GHC.Base
import GHC.Real
import Data.Word

-- | Return monotonic time in seconds, since some unspecified starting point
getMonotonicTime :: IO Double
getMonotonicTime = do w <- getMonotonicTimeNSec
                      return (fromIntegral w / 1000000000)

-- | Return monotonic time in nanoseconds, since some unspecified starting point
foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicTimeNSec :: IO Word64

