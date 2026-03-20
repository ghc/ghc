/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Posix implementation(s) of the interval timer for profiling and pre-emptive
 * scheduling.
 *
 * ---------------------------------------------------------------------------*/

/* The interval timer is used for profiling and for context switching.
 * This file defines the platform-specific services to install and run the
 * timers, and we call this the ticker. See rts/Timer.c for the
 * platform-dependent view of interval timing.
 *
 * Historically we had ticker implementations using signals. This was always a
 * rather shakey thing to do but we had few alternatives.
 * - One problem with using signals is that there are severe limits on what
 *   code can be called from signal handlers. In particular it's not possible
 *   to take locks in a signal handler contex. This was enough for contex
 *   switching, but it's no good for things like flushing the eventlog, or
 *   waking up rts tasks.
 * - We also want to avoid using alarm signals, as these can interrupt system
 *   calls (#10840) or can be overwritten by user code.
 */

/* Select a ticker implementation to use:
 *
 * On Linux we can use timerfd_* and a thread that waits on it using poll.
 * Linux has had timerfd since version 2.6.25. NetBSD has also had timerfd
 * support since version 10.
 *
 * For older version of linux/netbsd without timerfd, and for all other posix
 * platforms, we use the implementation using posix pthreads and nanosleep().
 */
#if defined(HAVE_SYS_TIMERFD_H)
#include "ticker/TimerFd.c"
#else
#include "ticker/Pthread.c"
#endif
