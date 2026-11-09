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

#include <unistd.h>
#include <time.h>

#if defined(solaris2_HOST_OS)
#include <sys/time.h>
#endif


#if !defined(HAVE_CLOCK_GETTIME)
#error We reqire clock_gettime. This should have been checked in configure.ac
#elif !defined(_POSIX_MONOTONIC_CLOCK)
#warning _POSIX_MONOTONIC_CLOCK not defined, which is likely to be a problem.
#endif

void initializeTimer(void)
{
}

static Time getClockTime(clockid_t clock)
{
    struct timespec ts;
    int res = clock_gettime(clock, &ts);
    if (RTS_LIKELY(res == 0)) {
        return SecondsToTime(ts.tv_sec) + NSToTime(ts.tv_nsec);
    } else {
        sysErrorBelch("clock_gettime");
        stg_exit(EXIT_FAILURE);
    }
}

Time getCurrentThreadCPUTime(void)
{
#if defined(solaris2_HOST_OS)
    // On OpenSolaris derivatives, the constant CLOCK_THREAD_CPUTIME_ID is
    // defined in a system header but it isn't actually usable. clock_gettime(2)
    // always returns EINVAL. Use solaris-specific gethrvtime(3) as an
    // alternative.
    return NSToTime(gethrvtime());
#else
    return getClockTime(CLOCK_THREAD_CPUTIME_ID);
#endif
}

Time getProcessCPUTime(void)
{
    return getClockTime(CLOCK_PROCESS_CPUTIME_ID);
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
    struct timespec ts;
    int res = clock_gettime(CLOCK_REALTIME, &ts);
    if (RTS_LIKELY(res == 0)) {
        *sec  = ts.tv_sec;
        *nsec = ts.tv_nsec;
    } else {
        sysErrorBelch("clock_gettime");
        stg_exit(EXIT_FAILURE);
    }
}
