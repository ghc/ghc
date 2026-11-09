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

#if defined(darwin_HOST_OS)
# include <mach/mach.h>
# include <mach/mach_time.h>
#endif
