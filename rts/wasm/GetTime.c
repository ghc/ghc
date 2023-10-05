/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

// Not POSIX, due to use of ru_majflt in getPageFaults()
// #include "rts/PosixSource.h"

#include "Rts.h"
#include "GetTime.h"

#include <time.h>
#include <sys/time.h>

void initializeTimer(void)
{
}

static Time getClockTime(clockid_t clock)
{
    struct timespec ts;
    int res = clock_gettime(clock, &ts);
    if (res == 0) {
        return SecondsToTime(ts.tv_sec) + NSToTime(ts.tv_nsec);
    } else {
        sysErrorBelch("clock_gettime");
        stg_exit(EXIT_FAILURE);
    }
}

Time getCurrentThreadCPUTime(void)
{
    return getClockTime(CLOCK_MONOTONIC);
}

Time getProcessCPUTime(void)
{
    return getClockTime(CLOCK_MONOTONIC);
}

StgWord64 getMonotonicNSec(void)
{
    return getClockTime(CLOCK_MONOTONIC);
}

Time getProcessElapsedTime(void)
{
    return NSToTime(getMonotonicNSec());
}

void getProcessTimes(Time *user, Time *elapsed)
{
    *user    = getProcessCPUTime();
    *elapsed = getProcessElapsedTime();
}

void getUnixEpochTime(StgWord64 *sec, StgWord32 *nsec)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    *sec  = tv.tv_sec;
    *nsec = tv.tv_usec * 1000;
}

W_
getPageFaults(void)
{
  return 0;
}
