{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

#include "HsFFI.h"
#include "HsBaseConfig.h"

-- For various _POSIX_* #defines
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

module System.CPUTime
    ( getCPUTime
    , cpuTimePrecision
    ) where

import System.IO.Unsafe (unsafePerformIO)

-- Here is where we decide which backend to use
#if defined(mingw32_HOST_OS)
import qualified System.CPUTime.Windows as I

#elif _POSIX_TIMERS > 0 && defined(_POSIX_CPUTIME) && _POSIX_CPUTIME >= 0
import qualified System.CPUTime.Posix.ClockGetTime as I

#elif defined(HAVE_GETRUSAGE) && ! solaris2_HOST_OS
import qualified System.CPUTime.Posix.RUsage as I

-- @getrusage()@ is right royal pain to deal with when targeting multiple
-- versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
-- while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
-- again in libucb in 2.6..)
--
-- Avoid the problem by resorting to times() instead.
#elif defined(HAVE_TIMES)
import qualified System.CPUTime.Posix.Times as I

#else
import qualified System.CPUTime.Unsupported as I
#endif

-- | The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.
cpuTimePrecision :: Integer
cpuTimePrecision = unsafePerformIO I.getCpuTimePrecision
{-# NOINLINE cpuTimePrecision #-}

-- | Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.
getCPUTime :: IO Integer
getCPUTime = I.getCPUTime
