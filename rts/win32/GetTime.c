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

// Number of ticks per second used by the QueryPerformanceFrequency
// implementaiton, represented by a 64-bit union type.
static LARGE_INTEGER qpc_frequency = {.QuadPart = 0};

// Initialize qpc_frequency. This function should be called before any call to
// getMonotonicNSec.  If QPC is not supported on this system, qpc_frequency is
// set to 0.
void initializeTimer()
{
    BOOL qpc_supported = QueryPerformanceFrequency(&qpc_frequency);
    if (!qpc_supported)
    {
        qpc_frequency.QuadPart = 0;
    }
}

HsWord64
getMonotonicNSec()
{
    if (qpc_frequency.QuadPart)
    {
        // system_time is a 64-bit union type used to represent the
        // tick count returned by QueryPerformanceCounter
        LARGE_INTEGER system_time;

        // get the tick count.
        QueryPerformanceCounter(&system_time);

        // compute elapsed seconds as double
        double secs = (double)system_time.QuadPart /
                      (double)qpc_frequency.QuadPart;

        // return elapsed time in nanoseconds
        return (HsWord64)(secs * 1e9);
    }
    else // fallback to GetTickCount
    {
        // NOTE: GetTickCount is a 32-bit millisecond value, so it wraps around
        // every 49 days.
        DWORD count = GetTickCount();

        // getTickCount is in milliseconds, so multiply it by 1000000 to get
        // nanoseconds.
        return (HsWord64)count * 1000000;
    }
}

Time
getProcessElapsedTime(void)
{
    return NSToTime(getMonotonicNSec());
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

W_
getPageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
    return 0;
}
