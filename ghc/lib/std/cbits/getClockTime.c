/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getClockTime.c,v 1.8 1999/10/26 09:34:09 sof Exp $
 *
 * getClockTime Runtime Support
 */

#ifndef _AIX
#define NON_POSIX_SOURCE    /* gettimeofday */
#endif

#include "Rts.h"
#include "stgio.h"

/* Note: skewing this code in favour of non-POSIX calls
   such as gettimeofday() and getclock() rather than time(),
   may seem strange/wrong. There's a good reason for it
   though - the non-POSIX calls gives you better precision --
   they return usecs (or nsecs) as well as seconds, which
   the users of getClockTime() is interested in knowing.
 */

#if defined(HAVE_GETTIMEOFDAY)
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif defined(HAVE_GETCLOCK)
# ifdef HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#elif defined(HAVE_TIME_H)
# include <time.h>
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#include <sys/types.h>
#include <sys/timeb.h>
#endif

StgInt
getClockTime(StgByteArray sec, StgByteArray nsec)
{
#if defined(_WIN32) && !defined(cygwin32_TARGET_OS)
  /*
   * ftime() as implemented by cygwin (in B20.1) is
   * not right, so stay away & use time() there instead.
   */
  struct timeb t;

  _ftime(&t);

  ((unsigned int *)sec)[0] = (unsigned int)t.time;
  ((unsigned int *)nsec)[0] = (unsigned int)t.millitm * 1000;
  return 0;
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval tp;
 
    if (gettimeofday(&tp, NULL) != 0) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((unsigned long int *)sec)[0] = tp.tv_sec;
    ((unsigned long int *)nsec)[0] = tp.tv_usec * 1000;
    return 0;
#elif defined(HAVE_GETCLOCK)
    struct timespec tp;

    if (getclock(TIMEOFDAY, &tp) != 0) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((unsigned long int *)sec)[0] = tp.tv_sec;
    ((unsigned long int *)nsec)[0] = tp.tv_nsec;
    return 0;
#elif defined(HAVE_TIME_H)
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((unsigned long int *)sec)[0] = t;
    ((unsigned long int *)nsec)[0] = 0;
    return 0;
#else
#error "getClockTime: don't know how to get at the clock's time"
#endif
}
