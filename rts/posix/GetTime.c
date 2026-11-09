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

#if defined(darwin_HOST_OS)
#include <mach/mach_time.h>
#include <mach/mach_init.h>
#include <mach/thread_act.h>
#include <mach/mach_port.h>
#endif

#if defined(solaris2_HOST_OS)
#include <sys/time.h>
#endif

#if defined(darwin_HOST_OS)
static uint64_t timer_scaling_factor_numer = 0;
static uint64_t timer_scaling_factor_denom = 0;
#endif

void initializeTimer(void)
{
#if defined(darwin_HOST_OS)
    mach_timebase_info_data_t info;
    (void) mach_timebase_info(&info);
    timer_scaling_factor_numer = (uint64_t)info.numer;
    timer_scaling_factor_denom = (uint64_t)info.denom;
#endif
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
    // N.B. Since macOS Catalina, Darwin supports clock_gettime but does not
    // support clock_getcpuclockid. Hence we prefer to use the Darwin-specific
    // path on Darwin, even if clock_gettime is available.
#if defined(darwin_HOST_OS)
    thread_basic_info_data_t info = { };
    mach_msg_type_number_t info_count = THREAD_BASIC_INFO_COUNT;
    kern_return_t kern_err = thread_info(mach_thread_self(), THREAD_BASIC_INFO,
                                         (thread_info_t) &info, &info_count);
    if (kern_err == KERN_SUCCESS) {
        return SecondsToTime(info.user_time.seconds) + USToTime(info.user_time.microseconds);
    } else {
        sysErrorBelch("getThreadCPUTime");
        stg_exit(EXIT_FAILURE);
    }
#elif defined(solaris2_HOST_OS)
    // On OpenSolaris derivatives, the constant CLOCK_THREAD_CPUTIME_ID is
    // defined in a system header but it isn't actually usable. clock_gettime(2)
    // always returns EINVAL. Use solaris-specific gethrvtime(3) as an
    // alternative.
    return NSToTime(gethrvtime());
#elif defined(HAVE_CLOCK_GETTIME)        &&  \
       defined(CLOCK_PROCESS_CPUTIME_ID) &&  \
       defined(HAVE_SYSCONF)
    static bool have_checked_usability = false;
    // The RELAXED operation is fine here as it's okay if we do the check below
    // more than once.
    if (!RELAXED_LOAD(&have_checked_usability)) {
        // The Linux clock_getres(2) manpage claims that some early versions of
        // Linux will return values which are uninterpretable in the presence
        // of migration across CPUs. They claim that clock_getcpuclockid(0)
        // will return ENOENT in this case. Check this.
        clockid_t clkid;
        if (clock_getcpuclockid(0, &clkid)) {
            sysErrorBelch("getCurrentThreadCPUTime: no supported");
            stg_exit(EXIT_FAILURE);
        }
        RELAXED_STORE(&have_checked_usability, true);
    }
    return getClockTime(CLOCK_THREAD_CPUTIME_ID);
#else
#error I know of no means to find the CPU time of current thread on this platform.
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
