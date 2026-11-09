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
