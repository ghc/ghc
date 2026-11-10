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

#include <time.h>

/* Convert FILETIMEs into secs */

static inline Time
fileTimeToRtsTime(FILETIME ft)
{
    Time t;
    t = ((Time)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
    t = NSToTime(t * 100);
    /* FILETIMES are in units of 100ns */
    return t;
}

Time
getCurrentThreadCPUTime(void)
{
    FILETIME creationTime, exitTime, userTime, kernelTime = {0,0};

    if (!GetThreadTimes(GetCurrentThread(), &creationTime,
                        &exitTime, &kernelTime, &userTime)) {
        sysErrorBelch("getCurrentThreadCPUTime: Win32 error %lu", GetLastError());
        return 0;
    }

    return fileTimeToRtsTime(userTime);
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

HsWord64
getMonotonicNSec(void)
{
    // https://learn.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps
    // The advice there is use QueryPerformanceCounter, and that this is now
    // always supported in Windows XP and later, and reliable from Vista.

    // Number of ticks per second, provided by QueryPerformanceFrequency
    static uint64_t ticks_per_sec = 0;

    // Initialize ticks_per_sec on the first call. It should not matter if two
    // threads race to do this. The result should be the same.
    if (RTS_UNLIKELY(RELAXED_LOAD_ALWAYS(&ticks_per_sec) == 0)) {
        LARGE_INTEGER qpc_frequency; // a 64-bit union type
        if (QueryPerformanceFrequency(&qpc_frequency)) {
            RELAXED_STORE_ALWAYS(&ticks_per_sec, qpc_frequency.QuadPart);
        } else {
            // This should be impossible now.
            sysErrorBelch("getMonotonicNSec: high res timestamps not available");
            stg_exit(EXIT_FAILURE);
        }
    }

    // Get the tick count.
    LARGE_INTEGER tick_count;
    QueryPerformanceCounter(&tick_count);

    // Compute elapsed time in nanoseconds. If avoiding float, we must multiply
    // by nanoseconds first (1e9) and then divide by ticks per sec. The
    // intermediate result can overflow a 64bit integer. So we use 128bit.
    return ((__uint128_t)tick_count.QuadPart * (__uint128_t)1000000000ull)
         / (__uint128_t)ticks_per_sec;
}

Time
getProcessElapsedTime(void)
{
    return NSToTime(getMonotonicNSec());
}

void
getUnixEpochTime(StgWord64 *sec, StgWord32 *nsec)
{
    /* Windows has a bunch of time APIs but none that directly give
       us unix epoch time, so we have to do a little dance. */

    FILETIME filetime;
    ULARGE_INTEGER unixtime;

    /* Windows FILETIME timestamps use an epoch-based time,
       using a 64bit unsigned word. The time is measured in
       units of 100 nanoseconds since an epoch of 1601. */
    GetSystemTimePreciseAsFileTime(&filetime);

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
