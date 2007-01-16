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

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef USE_PAPI
# include <papi.h>
#endif

#if ! ((defined(HAVE_GETRUSAGE) && !irix_HOST_OS) || defined(HAVE_TIMES))
#error No implementation for getProcessCPUTime() available.
#endif

#if defined(HAVE_GETTIMEOFDAY) && defined(HAVE_GETRUSAGE) && !irix_HOST_OS
// we'll implement getProcessCPUTime() and getProcessElapsedTime()
// separately, using getrusage() and gettimeofday() respectively

Ticks getProcessCPUTime(void)
{
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return ((Ticks)t.ru_utime.tv_sec * TICKS_PER_SECOND +
	    ((Ticks)t.ru_utime.tv_usec * TICKS_PER_SECOND)/1000000);
}

Ticks getProcessElapsedTime(void)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    return ((Ticks)tv.tv_sec * TICKS_PER_SECOND +
	    ((Ticks)tv.tv_usec * TICKS_PER_SECOND)/1000000);
}

void getProcessTimes(Ticks *user, Ticks *elapsed)
{
    *user    = getProcessCPUTime();
    *elapsed = getProcessElapsedTime();
}

#elif defined(HAVE_TIMES)

// we'll use the old times() API.

Ticks getProcessCPUTime(void)
{
#if !defined(THREADED_RTS) && USE_PAPI
    long long usec;
    if ((usec = PAPI_get_virt_usec()) < 0) {
	barf("PAPI_get_virt_usec: %lld", usec);
    }
    return ((usec * TICKS_PER_SECOND) / 1000000);
#else
    Ticks user, elapsed;
    getProcessTimes(&user,&elapsed);
    return user;
#endif
}

Ticks getProcessElapsedTime(void)
{
    Ticks user, elapsed;
    getProcessTimes(&user,&elapsed);
    return elapsed;
}

void getProcessTimes(Ticks *user, Ticks *elapsed)
{
    static nat ClockFreq = 0;

    if (ClockFreq == 0) {
#if defined(HAVE_SYSCONF)
	long ticks;
	ticks = sysconf(_SC_CLK_TCK);
	if ( ticks == -1 ) {
	    sysErrorBelch("sysconf");
	    stg_exit(EXIT_FAILURE);
	}
	ClockFreq = ticks;
#elif defined(CLK_TCK)		/* defined by POSIX */
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
    *user = (((Ticks)t.tms_utime * TICKS_PER_SECOND) / ClockFreq);
    *elapsed = (((Ticks)r * TICKS_PER_SECOND) / ClockFreq);
}

#endif // HAVE_TIMES

Ticks getThreadCPUTime(void)
{
#if USE_PAPI
    long long usec;
    if ((usec = PAPI_get_virt_usec()) < 0) {
	barf("PAPI_get_virt_usec: %lld", usec);
    }
    return ((usec * TICKS_PER_SECOND) / 1000000);

#elif defined(HAVE_CLOCK_GETTIME) && defined (_POSIX_THREAD_CPUTIME) && defined(CLOCK_THREAD_CPUTIME_ID) && defined(HAVE_SYSCONF)
    if (sysconf(_POSIX_THREAD_CPUTIME) != -1) {
        // clock_gettime() gives us per-thread CPU time.  It isn't
        // reliable on Linux, but it's the best we have.
        struct timespec ts;
        int res;
        res = clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
        if (res == 0) {
            return ((Ticks)ts.tv_sec * TICKS_PER_SECOND +
                    ((Ticks)ts.tv_nsec * TICKS_PER_SECOND) / 1000000000);
        }
    }
#endif
    return getProcessCPUTime();
}

nat
getPageFaults(void)
{
#if !defined(HAVE_GETRUSAGE) || irix_HOST_OS
    return 0;
#else
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
#endif
}

