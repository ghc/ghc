-- -----------------------------------------------------------------------------
-- $Id: CPUTime.hsc,v 1.1 2001/05/08 08:55:17 simonmar Exp $
--
-- (c) The University of Glasgow, 1995-2001
--

module CPUTime 
	(
         getCPUTime,       -- :: IO Integer
	 cpuTimePrecision  -- :: Integer
        ) where

import PrelMarshalAlloc
import PrelCTypesISO
import PrelCTypes
import PrelStorable
import PrelPtr

import PrelBase		( Int(..) )
import PrelByteArr  	( ByteArray(..), newIntArray )
import PrelArrExtra     ( unsafeFreezeByteArray )
import PrelIOBase	( IOException(..), 
			  IOErrorType( UnsupportedOperation ), 
			  unsafePerformIO, stToIO, ioException )
import Ratio

#include "config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef mingw32_TARGET_OS
# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
# endif
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if !defined(mingw32_TARGET_OS) && !defined(irix_TARGET_OS)
# if defined(HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
# endif
#endif

#ifdef hpux_TARGET_OS
#include <sys/syscall.h>
#define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#define HAVE_GETRUSAGE
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

-- -----------------------------------------------------------------------------
-- Computation `getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.

-- The `cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.

getCPUTime :: IO Integer
getCPUTime = do

#ifndef _WIN32
-- getrusage() is right royal pain to deal with when targetting multiple
-- versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
-- while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
-- again in libucb in 2.6..)
--
-- Avoid the problem by resorting to times() instead.
--
#if defined(HAVE_GETRUSAGE) && ! irix_TARGET_OS && ! solaris2_TARGET_OS
    allocaBytes (#const sizeof(struct rusage)) $ \ p_rusage -> do
    getrusage (#const RUSAGE_SELF) p_rusage

    let ru_utime = (#ptr struct rusage, ru_utime) p_rusage
    let ru_stime = (#ptr struct rusage, ru_stime) p_rusage
    u_sec  <- (#peek struct timeval,tv_sec)  ru_utime :: IO CLong
    u_usec <- (#peek struct timeval,tv_usec) ru_utime :: IO CLong
    s_sec  <- (#peek struct timeval,tv_sec)  ru_stime :: IO CLong
    s_usec <- (#peek struct timeval,tv_usec) ru_stime :: IO CLong

    return ((fromIntegral u_sec * 1000000 + fromIntegral u_usec + 
             fromIntegral s_sec * 1000000 + fromIntegral s_usec) 
		* 1000000)
#else
# if defined(HAVE_TIMES)
    allocaBytes (#const sizeof(struct tms)) $ \ p_tms ->
    times p_tms
    u_ticks  <- (#peek struct tms,tms_utime) p_tms :: IO CClock
    s_ticks  <- (#peek struct tms,tms_stime) p_tms :: IO CClock
    return (( (fromIntegral u_ticks + fromIntegral s_ticks) * 1000000000000) 
			`div` clockTicks)
# else
    ioException (IOError Nothing UnsupportedOperation 
			 "getCPUTime"
		         "can't get CPU time"
			 Nothing)
# endif
#endif

#else /* _WIN32 */

#error ToDo!!!

#ifdef _WIN32
/* 100ns units per sec, really */
#define NS_PER_SEC 10000000LL
#define FT2usecs(ll,ft)    \
    (ll)=(ft).dwHighDateTime; \
    (ll) <<= 32;              \
    (ll) |= (ft).dwLowDateTime;

#endif

/* cygwin32 or mingw32 version */
StgInt
getCPUTime(StgByteArray cpuStruct)
{
    FILETIME creationTime, exitTime, kernelTime, userTime;
    StgInt *cpu=(StgInt *)cpuStruct;
    unsigned long long uT, kT;
 
    /* ToDo: pin down elapsed times to just the OS thread(s) that
       are evaluating/managing Haskell code.
    */
    if (!GetProcessTimes (GetCurrentProcess(), &creationTime,
		          &exitTime, &kernelTime, &userTime)) {
	/* Probably on a Win95 box..*/
        cpu[0]=0;
        cpu[1]=0;
        cpu[2]=0;
        cpu[3]=0;
	return 1;
    }

    FT2usecs(uT, userTime);
    FT2usecs(kT, kernelTime);

    cpu[0] = (unsigned int)(uT / NS_PER_SEC);
    cpu[1] = (unsigned int)((uT - cpu[0] * NS_PER_SEC) * 100);
    cpu[2] = (unsigned int)(kT / NS_PER_SEC);
    cpu[3] = (unsigned int)((kT - cpu[2] * NS_PER_SEC) * 100);
    return 1;
}
#endif /* WIN32 */

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))

clockTicks :: Int
clockTicks =
#if defined(CLK_TCK)
    (#const CLK_TCK)
#else
    unsafePerformIO (sysconf (#const _SC_CLK_TCK) >>= return . fromIntegral)
#endif

type CRUsage = ()
foreign import unsafe getrusage :: CInt -> Ptr CRUsage -> IO CInt
foreign import unsafe sysconf   :: CInt -> IO CLong
