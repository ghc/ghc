/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "GetTime.h"

#include <time.h>
#include <sys/time.h>

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

Time getMonotonicTime(void)
{
    return getClockTime(CLOCK_MONOTONIC);
}

Time getWallClockTime(void)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    return SecondsToTime(tv.tv_sec) + USToTime(tv.tv_usec);
}
