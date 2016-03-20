{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}

#include "HsFFI.h"
#include "HsBaseConfig.h"

module System.CPUTime.Posix.RUsage
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import Data.Ratio
import Foreign
import Foreign.C
import System.CPUTime.Utils

-- For struct rusage
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

getCPUTime :: IO Integer
getCPUTime = allocaBytes (#const sizeof(struct rusage)) $ \ p_rusage -> do
    throwErrnoIfMinus1_ "getrusage" $ getrusage (#const RUSAGE_SELF) p_rusage

    let ru_utime = (#ptr struct rusage, ru_utime) p_rusage
    let ru_stime = (#ptr struct rusage, ru_stime) p_rusage
    u_sec  <- (#peek struct timeval,tv_sec)  ru_utime :: IO CTime
    u_usec <- (#peek struct timeval,tv_usec) ru_utime :: IO CSUSeconds
    s_sec  <- (#peek struct timeval,tv_sec)  ru_stime :: IO CTime
    s_usec <- (#peek struct timeval,tv_usec) ru_stime :: IO CSUSeconds
    let usec = cTimeToInteger u_sec * 1e6 + csuSecondsToInteger u_usec +
               cTimeToInteger s_sec * 1e6 + csuSecondsToInteger s_usec
    return (usec * 1e6)

type CRUsage = ()
foreign import capi unsafe "HsBase.h getrusage" getrusage :: CInt -> Ptr CRUsage -> IO CInt

getCpuTimePrecision :: IO Integer
getCpuTimePrecision =
    return $ round ((1e12::Integer) % fromIntegral clk_tck)

foreign import ccall unsafe clk_tck :: CLong
