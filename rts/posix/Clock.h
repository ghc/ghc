/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2012
 *
 * Posix monotonic clock
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(HAVE_UNISTD_H)
# include <unistd.h>
#endif

#include <time.h>

#if defined(HAVE_SYS_TIME_H)
# include <sys/time.h>
#endif

#define RTS_CLOCK_MONOTONIC CLOCK_MONOTONIC
#define RTS_CLOCK_REALTIME  CLOCK_REALTIME
#define RTS_CLOCK_CPUTIME_THREAD CLOCK_THREAD_CPUTIME_ID
