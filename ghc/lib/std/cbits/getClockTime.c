/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getClockTime.c,v 1.4 1999/05/03 13:22:29 sof Exp $
 *
 * getClockTime Runtime Support
 */

#ifndef _AIX
#define NON_POSIX_SOURCE    /* gettimeofday */
#endif

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_GETCLOCK

# ifdef HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif

#else
# ifdef HAVE_GETTIMEOFDAY

#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif

# else

#  ifdef HAVE_TIME_H
#   include <time.h>
#  endif

# endif
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#include <sys/types.h>
#include <sys/timeb.h>
#endif

StgInt
getClockTime(StgByteArray sec, StgByteArray nsec)
{
#if defined(_WIN32)
  struct timeb t;

  _ftime(&t);

  ((unsigned int *)sec)[0] = (unsigned int)t.time;
  ((unsigned int *)nsec)[0] = (unsigned int)t.millitm * 1000;
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
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((unsigned long int *)sec)[0] = t;
    ((unsigned long int *)nsec)[0] = 0;
    return 0;
#endif
}

StgInt
prim_getClockTime(StgByteArray sec, StgByteArray nsec)
{
#if defined(_WIN32)
  struct timeb t;

  _ftime(&t);

  ((unsigned long int *)sec)[0] = t.time;
  ((unsigned long int *)nsec)[0] = t.millitm * 1000;
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
    time_t t;
    if ((t = time(NULL)) == (time_t) -1) {
	cvtErrno();
	stdErrno();
	return -1;
    }
    ((StgInt64*)sec)[0] = t;
    ((StgInt64*)nsec)[0] = 0;
    return 0;
#endif
}
