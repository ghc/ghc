/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "GetTime.h"

#include <windows.h>

#ifdef HAVE_TIME_H
# include <time.h>
#endif

/* Convert FILETIMEs into secs */

static INLINE_ME Time
fileTimeToRtsTime(FILETIME ft)
{
    Time t;
    t = ((Time)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
    t = NSToTime(t * 100);
    /* FILETIMES are in units of 100ns */
    return t;
}    

void
getProcessTimes(Time *user, Time *elapsed)
{
    *user    = getProcessCPUTime();
    *elapsed = getProcessElapsedTime();
}

Time
getProcessCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (!GetProcessTimes(GetCurrentProcess(), &creationTime,
			 &exitTime, &kernelTime, &userTime)) {
	return 0;
    }

    return fileTimeToRtsTime(userTime);
}

// getProcessElapsedTime relies on QueryPerformanceFrequency 
// which should be available on any Windows computer thay you
// would want to run Haskell on. Satnam Singh, 5 July 2010.

Time
getProcessElapsedTime(void)
{
    // frequency represents the number of ticks per second
    // used by the QueryPerformanceFrequency implementaiton
    // and is represented by a 64-bit union type initially set to 0
    // and updated just once (hence use of static).
    static LARGE_INTEGER frequency = {.QuadPart = 0} ;  

    // system_time is a 64-bit union type used to represent the
    // tick count returned by QueryPerformanceCounter
    LARGE_INTEGER system_time ;

    // If this is the first time we are calling getProcessElapsedTime
    // then record the ticks per second used by QueryPerformanceCounter
    if (frequency.QuadPart == 0) {
      QueryPerformanceFrequency(&frequency);
    }
    
    // Get the tick count.
    QueryPerformanceCounter(&system_time) ;

    // Return the tick count as a Time value.
    // Using double to compute the intermediate value, because a 64-bit
    // int would overflow when multiplied by TICK_RESOLUTION in about 81 days.
    return fsecondsToTime((double)system_time.QuadPart /
                          (double)frequency.QuadPart) ;
}

Time
getThreadCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (!GetThreadTimes(GetCurrentThread(), &creationTime,
			&exitTime, &kernelTime, &userTime)) {
	return 0;
    }

    return fileTimeToRtsTime(userTime);
}

void
getUnixEpochTime(StgWord64 *sec, StgWord32 *nsec)
{
    /* Windows has a bunch of time APIs but none that directly give
       us unix epoch time, so we have to do a little dance. */

    SYSTEMTIME systime;
    FILETIME filetime;
    ULARGE_INTEGER unixtime;

    /* Windows SYSTEMTIME is a big struct with fields for
       year, month, day, hour, minute, second, millisecond. */
    GetSystemTime(&systime);
    /* Windows FILETIME timestamps use an epoch-based time,
       using a 64bit unsigned word. The time is measured in
       units of 100 nanoseconds since an epoch of 1601. */
    SystemTimeToFileTime(&systime, &filetime);

    /* FILETIME isn't directly a 64bit word, but a struct with
       a pair of 32bit words, so we have to convert via a
       ULARGE_INTEGER struct which is a handy union type */
    unixtime.LowPart  = filetime.dwLowDateTime;
    unixtime.HighPart = filetime.dwHighDateTime;
    
    /* We have to do an epoch conversion, since FILETIME uses 1601
       while we want unix epoch of 1970. In case you were wondering,
       there were 11,644,473,600 seconds between 1601 and 1970, then
       multiply by 10^7 for units of 100 nanoseconds. */
    unixtime.QuadPart = unixtime.QuadPart - 116444736000000000ull;
    
    /* For the seconds part we use integer division by 10^7 */
    *sec  = unixtime.QuadPart / 10000000ull;
    
    /* The remainder from integer division by 10^7 gives us
       the sub-second component in units of 100 nanoseconds.
       So for nanoseconds we just multiply by 100.
       Note that nanoseconds always fits in a 32bit word */
    *nsec = ((unsigned long)(unixtime.QuadPart % 10000000ull)) * 100ul;
}

nat
getPageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
    return 0;
}
