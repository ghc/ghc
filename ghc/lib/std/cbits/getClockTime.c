/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getClockTime.c,v 1.5 1999/09/12 14:33:56 sof Exp $
 *
 * getClockTime Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

/* It seems morally wrong to skew this in favour of
   using non-POSIX calls (gettimeofday(), ftime()..),
   rather than time()....so, let's re-order it all
   (and hope OS idiosyncracies won't get in the way
   of using time(), the moral elite's favourite.)
 */

#if defined(HAVE_TIME_H)
# include <time.h>
#elif defined(HAVE_GETCLOCK)
# ifdef HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#elif defined(HAVE_GETTIMEOFDAY)
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
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
   * time() as is implemented by cygwin (in B20.1) is
   * not right, so stay away (and use time()) instead.
   */
  struct timeb t;

  _ftime(&t);

  ((unsigned int *)sec)[0] = (unsigned int)t.time;
  ((unsigned int *)nsec)[0] = (unsigned int)t.millitm * 1000;
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
#else
#error "getClockTime: don't know how to get at the clock's time"
#endif
}

StgInt
prim_getClockTime(StgByteArray sec, StgByteArray nsec)
{
#if defined(_WIN32) && !defined(cygwin32_TARGET_OS)
  /* see getClockTime() comment re: ftime() & cygwin */
  struct timeb t;

  _ftime(&t);

  ((unsigned long int *)sec)[0] = t.time;
  ((unsigned long int *)nsec)[0] = t.millitm * 1000;
  return 0;
#elif defined(HAVE_TIME_H)
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((StgInt64*)sec)[0] = t;
    ((StgInt64*)nsec)[0] = 0;
    return 0;
#elif defined(HAVE_GETCLOCK)
    struct timespec tp;

    if (getclock(TIMEOFDAY, &tp) != 0) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((StgInt64*)sec)[0] = tp.tv_sec;
    ((StgInt64*)nsec)[0] = tp.tv_nsec;
    return 0;
#elif defined(HAVE_GETTIMEOFDAY)
    struct timeval tp;
 
    if (gettimeofday(&tp, NULL) != 0) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((StgInt64*)sec)[0] = tp.tv_sec;
    ((StgInt64*)nsec)[0] = tp.tv_usec * 1000;
    return 0;
#else
#error "getClockTime: don't know how to get at the clock's time"
#endif
}
