/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getCPUTime.c,v 1.5 1999/05/03 13:22:29 sof Exp $
 *
 * getCPUTime Runtime Support
 */

#ifndef _AIX
#define NON_POSIX_SOURCE /*needed for solaris2 only?*/
#endif

/* how is this to work given we have not read platform.h yet? */
#ifdef hpux_TARGET_OS
#define _INCLUDE_HPUX_SOURCE
#endif

#include "Rts.h"

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

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#ifdef hpux_TARGET_OS
#include <sys/syscall.h>
#define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#define HAVE_GETRUSAGE
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

StgInt 
clockTicks ()
{
 return (
#if defined(CLK_TCK)
    CLK_TCK
#else
    sysconf(_SC_CLK_TCK)
#endif
    ); 
}

/* 
 * Our caller wants a pointer to four StgInts,
 * user seconds, user nanoseconds, system seconds, system nanoseconds.
 * Yes, the timerval has unsigned components, but nanoseconds take only
 * 30 bits, and our CPU usage would have to be over 68 years for the 
 * seconds to overflow 31 bits.
 */

#ifndef _WIN32
StgByteArray
getCPUTime(StgByteArray cpuStruct)
{
    StgInt *cpu=(StgInt *)cpuStruct;

/* getrusage() is right royal pain to deal with when targetting multiple
   versions of Solaris, since some versions supply it in libc (2.3 and 2.5),
   while 2.4 has got it in libucb (I wouldn't be too surprised if it was back
   again in libucb in 2.6..)

   Avoid the problem by resorting to times() instead.
*/
#if defined(HAVE_GETRUSAGE) && ! irix_TARGET_OS && ! solaris2_TARGET_OS
    struct rusage t;

    getrusage(RUSAGE_SELF, &t);
    cpu[0] = t.ru_utime.tv_sec;
    cpu[1] = 1000 * t.ru_utime.tv_usec;
    cpu[2] = t.ru_stime.tv_sec;
    cpu[3] = 1000 * t.ru_stime.tv_usec;

#else
# if defined(HAVE_TIMES)
    struct tms t;
#  if defined(CLK_TCK)
#   define ticks CLK_TCK
#  else
    long ticks;
    ticks = sysconf(_SC_CLK_TCK);
#  endif

    times(&t);
    cpu[0] = t.tms_utime / ticks;
    cpu[1] = (t.tms_utime - cpu[0] * ticks) * (1000000000 / ticks);
    cpu[2] = t.tms_stime / ticks;
    cpu[3] = (t.tms_stime - cpu[2] * ticks) * (1000000000 / ticks);

# else
    return NULL;
# endif
#endif
    return (StgByteArray) cpuStruct;
}

#else

#ifdef _WIN32
/* 100ns units per sec, really */
#define NS_PER_SEC 10000000LL
#define FT2usecs(ll,ft)    \
    (ll)=(ft).dwHighDateTime; \
    (ll) <<= 32;              \
    (ll) |= (ft).dwLowDateTime;

#endif

/* cygwin32 or mingw32 version */
StgByteArray
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
	return (StgByteArray)cpu;
    }

    FT2usecs(uT, userTime);
    FT2usecs(kT, kernelTime);
    
    cpu[0] = (unsigned int)(uT / NS_PER_SEC);
    cpu[1] = (unsigned int)(uT * 100);
    cpu[0] = (unsigned int)(kT / NS_PER_SEC);
    cpu[1] = (unsigned int)(kT * 100);
    return (StgByteArray)cpu;
}

#endif /* _WIN32 */
