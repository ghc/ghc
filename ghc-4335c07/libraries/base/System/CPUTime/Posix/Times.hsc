{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}

#include "HsFFI.h"
#include "HsBaseConfig.h"

module System.CPUTime.Posix.Times
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import Data.Ratio
import Foreign
import Foreign.C
import System.CPUTime.Utils

-- for struct tms
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

getCPUTime :: IO Integer
getCPUTime = allocaBytes (#const sizeof(struct tms)) $ \ p_tms -> do
    _ <- times p_tms
    u_ticks  <- (#peek struct tms,tms_utime) p_tms :: IO CClock
    s_ticks  <- (#peek struct tms,tms_stime) p_tms :: IO CClock
    return (( (cClockToInteger u_ticks + cClockToInteger s_ticks) * 1e12)
                        `div` fromIntegral clockTicks)

type CTms = ()
foreign import ccall unsafe times :: Ptr CTms -> IO CClock

getCpuTimePrecision :: IO Integer
getCpuTimePrecision =
    return $ round ((1e12::Integer) % clockTicks)

foreign import ccall unsafe clk_tck :: CLong

clockTicks :: Integer
clockTicks = fromIntegral clk_tck
