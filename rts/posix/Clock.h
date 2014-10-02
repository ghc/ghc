/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2012
 *
 * Posix monotonic clock
 *
 * ---------------------------------------------------------------------------*/

#ifndef POSIX_CLOCK_H
#define POSIX_CLOCK_H

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_CLOCK_GETTIME
# ifdef _POSIX_MONOTONIC_CLOCK
#  define CLOCK_ID CLOCK_MONOTONIC
# else
#  define CLOCK_ID CLOCK_REALTIME
# endif
#elif defined(darwin_HOST_OS)
# include <mach/mach.h>
# include <mach/mach_time.h>
#endif

#endif /* POSIX_CLOCK_H */
