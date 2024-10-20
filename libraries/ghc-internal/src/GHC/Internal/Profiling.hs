{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | @since base-4.7.0.0
module GHC.Internal.Profiling ( -- * Cost Centre Profiling
                       startProfTimer
                     , stopProfTimer
                       -- * Heap Profiling
                     , startHeapProfTimer
                     , stopHeapProfTimer
                     , requestHeapCensus
                       -- * Ticky counters (eventlog)
                     , requestTickyCounterSamples
                     )where

import GHC.Internal.Base

-- | Stop attributing ticks to cost centres. Allocations will still be
-- attributed.
--
-- @since base-4.7.0.0
foreign import ccall stopProfTimer :: IO ()

-- | Start attributing ticks to cost centres. This is called by the RTS on
-- startup but can be disabled using the rts flag @--no-automatic-time-samples@.
--
-- @since base-4.7.0.0
foreign import ccall startProfTimer :: IO ()

-- | Request a heap census on the next context switch. The census can be
-- requested whether or not the heap profiling timer is running.
--
-- Note: This won't do anything unless you also specify a profiling mode on the
-- command line using the normal RTS options.
--
-- @since base-4.16.0.0
foreign import ccall requestHeapCensus :: IO ()

-- | Start heap profiling. This is called normally by the RTS on start-up,
-- but can be disabled using the rts flag @--no-automatic-heap-samples@.
--
-- Note: This won't do anything unless you also specify a profiling mode on the
-- command line using the normal RTS options.
--
-- @since base-4.16.0.0
foreign import ccall startHeapProfTimer :: IO ()

-- | Stop heap profiling.
--
-- Note: This won't do anything unless you also specify a profiling mode on the
-- command line using the normal RTS options.
--
-- @since base-4.16.0.0
foreign import ccall stopHeapProfTimer :: IO ()

-- | Request ticky counter samples to be written to the eventlog.
--
-- Note: This won't do anything unless you have specified RTS options on
-- the command line to log ticky samples to the eventlog.
--
-- @since base-4.20.0.0

foreign import ccall requestTickyCounterSamples :: IO ()
