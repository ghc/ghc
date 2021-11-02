{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}

#include "HsFFI.h"
#include "HsBaseConfig.h"
#if HAVE_TIME_H
#include <unistd.h>
#include <time.h>
#endif

module System.CPUTime.Posix.ClockGetTime
    ( getCPUTime
    , getCpuTimePrecision
    ) where

#if _POSIX_TIMERS > 0 && defined(_POSIX_CPUTIME) && _POSIX_CPUTIME >= 0

import Foreign
import Foreign.C
import System.CPUTime.Utils

getCPUTime :: IO Integer
getCPUTime = fmap snd $ withTimespec $ \ts ->
    throwErrnoIfMinus1_ "clock_gettime"
    $ clock_gettime cLOCK_PROCESS_CPUTIME_ID ts

getCpuTimePrecision :: IO Integer
getCpuTimePrecision = fmap snd $ withTimespec $ \ts ->
    throwErrnoIfMinus1_ "clock_getres"
    $ clock_getres cLOCK_PROCESS_CPUTIME_ID ts

data Timespec

-- | Perform the given action to fill in a @struct timespec@, returning the
-- result of the action and the value of the @timespec@ in picoseconds.
withTimespec :: (Ptr Timespec -> IO a) -> IO (a, Integer)
withTimespec action =
    allocaBytes (# const sizeof(struct timespec)) $ \p_ts -> do
        r <- action p_ts
        u_sec  <- (#peek struct timespec,tv_sec)  p_ts :: IO CTime
        u_nsec <- (#peek struct timespec,tv_nsec) p_ts :: IO CLong
        return (r, cTimeToInteger u_sec * 1e12 + fromIntegral u_nsec * 1e3)

foreign import capi unsafe "time.h clock_getres"  clock_getres  :: CUIntPtr -> Ptr Timespec -> IO CInt
foreign import capi unsafe "time.h clock_gettime" clock_gettime :: CUIntPtr -> Ptr Timespec -> IO CInt
foreign import capi unsafe "time.h value CLOCK_PROCESS_CPUTIME_ID" cLOCK_PROCESS_CPUTIME_ID :: CUIntPtr

#else

-- This should never happen
getCPUTime :: IO Integer
getCPUTime = error "System.CPUTime.Posix.ClockGetTime: Unsupported"

getCpuTimePrecision :: IO Integer
getCpuTimePrecision = error "System.CPUTime.Posix.ClockGetTime: Unsupported"

#endif // _POSIX_CPUTIME
