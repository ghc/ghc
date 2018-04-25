{-# LANGUAGE CPP, CApiFFI, NondecreasingIndentation, NumDecimals #-}

#include "HsFFI.h"
#include "HsBaseConfig.h"

module System.CPUTime.Windows
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import Foreign
import Foreign.C

-- For FILETIME etc. on Windows
#if HAVE_WINDOWS_H
#include <windows.h>
#endif

getCPUTime :: IO Integer
getCPUTime = do
     -- NOTE: GetProcessTimes() is only supported on NT-based OSes.
     -- The counts reported by GetProcessTimes() are in 100-ns (10^-7) units.
    allocaBytes (#const sizeof(FILETIME)) $ \ p_creationTime -> do
    allocaBytes (#const sizeof(FILETIME)) $ \ p_exitTime -> do
    allocaBytes (#const sizeof(FILETIME)) $ \ p_kernelTime -> do
    allocaBytes (#const sizeof(FILETIME)) $ \ p_userTime -> do
    pid <- getCurrentProcess
    ok <- getProcessTimes pid p_creationTime p_exitTime p_kernelTime p_userTime
    if toBool ok then do
      ut <- ft2psecs p_userTime
      kt <- ft2psecs p_kernelTime
      return (ut + kt)
     else return 0
  where
        ft2psecs :: Ptr FILETIME -> IO Integer
        ft2psecs ft = do
          high <- (#peek FILETIME,dwHighDateTime) ft :: IO Word32
          low  <- (#peek FILETIME,dwLowDateTime)  ft :: IO Word32
            -- Convert 100-ns units to picosecs (10^-12)
            -- => multiply by 10^5.
          return (((fromIntegral high) * (2^(32::Int)) + (fromIntegral low)) * 100000)

    -- ToDo: pin down elapsed times to just the OS thread(s) that
    -- are evaluating/managing Haskell code.

-- While it's hard to get reliable numbers, the consensus is that Windows only provides
-- 16 millisecond resolution in GetProcessTimes (see Python PEP 0418)
getCpuTimePrecision :: IO Integer
getCpuTimePrecision = return 16e9

type FILETIME = ()
type HANDLE = ()

-- need proper Haskell names (initial lower-case character)
#if defined(i386_HOST_ARCH)
foreign import stdcall unsafe "GetCurrentProcess" getCurrentProcess :: IO (Ptr HANDLE)
foreign import stdcall unsafe "GetProcessTimes" getProcessTimes :: Ptr HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO CInt
#elif defined(x86_64_HOST_ARCH)
foreign import ccall unsafe "GetCurrentProcess" getCurrentProcess :: IO (Ptr HANDLE)
foreign import ccall unsafe "GetProcessTimes" getProcessTimes :: Ptr HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO CInt
#else
#error Unknown mingw32 arch
#endif
