/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent time measurement functions
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
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

#if ! ((defined(HAVE_GETRUSAGE) && !irix_HOST_OS) || defined(HAVE_TIMES))
#error No implementation for getProcessCPUTime() available.
#endif

#if (defined(HAVE_GETTIMEOFDAY) && defined(HAVE_GETRUSAGE) && !irix_HOST_OS)
// we'll implement getProcessCPUTime() and getProcessElapsedTime()
// separately, using getrusage() and gettimeofday() respectively

Ticks getProcessCPUTime(void)
{
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return (t.ru_utime.tv_sec * TICKS_PER_SECOND + 
	    ((Ticks)t.ru_utime.tv_usec * TICKS_PER_SECOND)/1000000);
}

Ticks getProcessElapsedTime(void)
{
    struct timeval tv;
    gettimeofday(&tv, (struct timezone *) NULL);
    return (tv.tv_sec * TICKS_PER_SECOND +
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
    Ticks user, elapsed;
    getProcessTimes(&user,&elapsed);
    return user;
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
	    errorBelch("sysconf\n");
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
#ifdef HAVE_CLOCK_GETTIME
    // clock_gettime() gives us per-thread CPU time.  It isn't
    // reliable on Linux, but it's the best we have.
    struct timespec ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    return (ts.tv_sec * TICKS_PER_SECOND + 
	    ts.tv_nsec / (1000000000/TICKS_PER_SECOND));
#else
    return getProcessCPUTime();
#endif
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

