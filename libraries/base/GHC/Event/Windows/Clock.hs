{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.Clock (
    Clock,
    Seconds,
    getTime,
    getClock,

    -- * Specific implementations
    queryPerformanceCounter,
    getTickCount64
) where

import qualified GHC.Event.Windows.FFI as FFI

import Data.Maybe
import GHC.Base
import GHC.Real

-- | Monotonic clock
newtype Clock = Clock (IO Seconds)

type Seconds = Double

-- | Get the current time, in seconds since some fixed time in the past.
getTime :: Clock -> IO Seconds
getTime (Clock io) = io

-- | Figure out what time API to use, and return a 'Clock' for accessing it.
getClock :: IO Clock
getClock = tryInOrder
           [ queryPerformanceCounter
           , fmap Just getTickCount64
           ]

tryInOrder :: Monad m => [m (Maybe a)] -> m a
tryInOrder (x:xs) = x >>= maybe (tryInOrder xs) return
tryInOrder []     = undefined

mapJust :: Monad m => m (Maybe a) -> (a -> b) -> m (Maybe b)
mapJust m f = liftM (fmap f) m

queryPerformanceCounter :: IO (Maybe Clock)
queryPerformanceCounter =
    FFI.queryPerformanceFrequency `mapJust` \freq ->
    Clock $! do
        count <- FFI.queryPerformanceCounter
        let !secs = fromIntegral count / fromIntegral freq
        return secs

getTickCount64 :: IO Clock
getTickCount64 =
    return $! Clock $! do
      msecs <- FFI.getTickCount64
      return $! fromIntegral msecs / 1000
