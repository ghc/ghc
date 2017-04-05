/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

// Not POSIX, due to use of ru_majflt in getPageFaults()
// #include "PosixSource.h"

#include "Rts.h"
#include "GetTime.h"
#include "Clock.h"

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#if ! (defined(HAVE_GETRUSAGE) || defined(HAVE_TIMES))
#error No implementation for getProcessCPUTime() available.
#endif

#if defined(HAVE_GETTIMEOFDAY) && defined(HAVE_GETRUSAGE)
// we'll implement getProcessCPUTime() and getProcessElapsedTime()
// separately, using getrusage() and gettimeofday() respectively

#if !defined(HAVE_CLOCK_GETTIME) && defined(darwin_HOST_OS)
static uint64_t timer_scaling_factor_numer = 0;
static uint64_t timer_scaling_factor_denom = 0;
#endif

void initializeTimer()
{
#if !defined(HAVE_CLOCK_GETTIME) && defined(darwin_HOST_OS)
    mach_timebase_info_data_t info;
    (void) mach_timebase_info(&info);
    timer_scaling_factor_numer = (uint64_t)info.numer;
    timer_scaling_factor_denom = (uint64_t)info.denom;
#endif
}

Time getProcessCPUTime(void)
{
#if !defined(BE_CONSERVATIVE)            &&  \
       defined(HAVE_CLOCK_GETTIME)       &&  \
       defined(_SC_CPUTIME)             &&  \
       defined(CLOCK_PROCESS_CPUTIME_ID) &&  \
       defined(HAVE_SYSCONF)
    static int checked_sysconf = 0;
    static int sysconf_result = 0;

    if (!checked_sysconf) {
        sysconf_result = sysconf(_SC_CPUTIME);
        checked_sysconf = 1;
    }
    if (sysconf_result != -1) {
        struct timespec ts;
        int res;
        res = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
        if (res == 0) {
            return SecondsToTime(ts.tv_sec) + NSToTime(ts.tv_nsec);
        } else {
            sysErrorBelch("clock_gettime");
            stg_exit(EXIT_FAILURE);
        }
    }
#endif

    // fallback to getrusage
    {
        struct rusage t;
        getrusage(RUSAGE_SELF, &t);
        return SecondsToTime(t.ru_utime.tv_sec) + USToTime(t.ru_utime.tv_usec);
    }
}

StgWord64 getMonotonicNSec(void)
{
#if defined(HAVE_CLOCK_GETTIME)
    struct timespec ts;
    int res;

    res = clock_gettime(CLOCK_ID, &ts);
    if (res != 0) {
        sysErrorBelch("clock_gettime");
        stg_exit(EXIT_FAILURE);
    }
    return (StgWord64)ts.tv_sec * 1000000000 +
           (StgWord64)ts.tv_nsec;

#elif defined(darwin_HOST_OS)

    uint64_t time = mach_absolute_time();
    return (time * timer_scaling_factor_numer) / timer_scaling_factor_denom;

#else // use gettimeofday()

    struct timeval tv;

    gettimeofday(&tv, (struct timezone *) NULL);
    return (StgWord64)tv.tv_sec * 1000000000 +
           (StgWord64)tv.tv_usec * 1000;

#endif
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

#elif defined(HAVE_TIMES)

// we'll use the old times() API.

Time getProcessCPUTime(void)
{
    Time user, elapsed;
    getProcessTimes(&user,&elapsed);
    return user;
}

Time getProcessElapsedTime(void)
{
    Time user, elapsed;
    getProcessTimes(&user,&elapsed);
    return elapsed;
}

void getProcessTimes(Time *user, Time *elapsed)
{
    static uint32_t ClockFreq = 0;

    if (ClockFreq == 0) {
#if defined(HAVE_SYSCONF)
        long ticks;
        ticks = sysconf(_SC_CLK_TCK);
        if ( ticks == -1 ) {
            sysErrorBelch("sysconf");
            stg_exit(EXIT_FAILURE);
        }
        ClockFreq = ticks;
#elif defined(CLK_TCK)          /* defined by POSIX */
        ClockFreq = CLK_TCK;
#elif defined(HZ)
        ClockFreq = HZ;
#elif defined(CLOCKS_PER_SEC)
        ClockFreq = CLOCKS_PER_SEC;
#else
        errorBelch("can't get clock resolution");
        stg_exit(EXIT_FAILURE);
#endif
    }

    struct tms t;
    clock_t r = times(&t);
    *user = SecondsToTime(t.tms_utime) / ClockFreq;
    *elapsed = SecondsToTime(r) / ClockFreq;
}

#endif // HAVE_TIMES

void getUnixEpochTime(StgWord64 *sec, StgWord32 *nsec)
{
#if defined(HAVE_GETTIMEOFDAY)
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    *sec  = tv.tv_sec;
    *nsec = tv.tv_usec * 1000;
#else
    /* Sigh, fall back to second resolution. */
    time_t t;
    time(&t);
    *sec  = t;
    *nsec = 0;
#endif
}

W_
getPageFaults(void)
{
#if !defined(HAVE_GETRUSAGE) || haiku_HOST_OS
    return 0;
#else
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
#endif
}
