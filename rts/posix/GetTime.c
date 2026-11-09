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
#include "Clock.h"

#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif

#if defined(HAVE_SYS_TIMES_H)
# include <sys/times.h>
#endif

#if !defined(HAVE_CLOCK_GETTIME)
#error We reqire clock_gettime. This should have been checked in configure.ac
#elif !defined(_POSIX_MONOTONIC_CLOCK)
#warning _POSIX_MONOTONIC_CLOCK not defined, which is likely to be a problem.
#endif

#if ! (defined(HAVE_GETRUSAGE) || defined(HAVE_TIMES))
#error No implementation for getProcessCPUTime() available.
#endif

#if defined(solaris2_HOST_OS)
#include <sys/time.h>
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
#if !defined(BE_CONSERVATIVE)            &&  \
       defined(HAVE_CLOCK_GETTIME)       &&  \
       defined(_SC_CPUTIME)              &&  \
       defined(CLOCK_PROCESS_CPUTIME_ID) &&  \
       defined(HAVE_SYSCONF)
    static int checked_sysconf = 0;
    static int sysconf_result = 0;

    if (!checked_sysconf) {
        sysconf_result = sysconf(_SC_CPUTIME);
        checked_sysconf = 1;
    }
    if (sysconf_result != -1) {
        return getClockTime(CLOCK_PROCESS_CPUTIME_ID);
    }
#endif

    // fallback to getrusage
    {
        struct rusage t;
        getrusage(RUSAGE_SELF, &t);
        return SecondsToTime(t.ru_utime.tv_sec + t.ru_stime.tv_sec)
                + USToTime(t.ru_utime.tv_usec + t.ru_stime.tv_usec);
    }
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

W_
getPageFaults(void)
{
#if !defined(HAVE_GETRUSAGE) || defined(haiku_HOST_OS)
    return 0;
#else
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
#endif
}
