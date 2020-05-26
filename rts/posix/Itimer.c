/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2007
 *
 * Interval timer for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * The interval timer is used for profiling and for context switching in the
 * threaded build.  Though POSIX 1003.1b includes a standard interface for
 * such things, no one really seems to be implementing them yet.  Even
 * Solaris 2.3 only seems to provide support for @CLOCK_REAL@, whereas we're
 * keen on getting access to @CLOCK_VIRTUAL@.
 *
 * Hence, we often use the old-fashioned @setitimer@ that just about everyone
 * seems to support.  So much for standards.
 */

#include "PosixSource.h"
#include "Rts.h"

/*
 * timer_create doesn't exist and setitimer doesn't fire on iOS, so we're using
 * a pthreads-based implementation. It may be to do with interference with the
 * signals of the debugger. Revisit. See #7723.
 */
#if defined(ios_HOST_OS)
#define USE_PTHREAD_FOR_ITIMER
#endif

/*
 * We want to avoid using the SIGALRM signals whenever possible as these signals
 * interrupt system calls (see #10840) and can be overridden by user code. On
 * Darwin we can use a dedicated thread and usleep.
 */
#if defined(darwin_HOST_OS)
#define USE_PTHREAD_FOR_ITIMER
#endif

/*
 * On Linux in the threaded RTS we can use timerfd_* (introduced in Linux
 * 2.6.25) and a thread instead of alarm signals. It avoids the risk of
 * interrupting syscalls (see #10840) and the risk of being accidentally
 * modified in user code using signals.
 */
#if defined(linux_HOST_OS) && defined(THREADED_RTS) && HAVE_SYS_TIMERFD_H
#define USE_PTHREAD_FOR_ITIMER
#endif

#if defined(freebsd_HOST_OS)
#define USE_PTHREAD_FOR_ITIMER
#endif

#if defined(solaris2_HOST_OS)
/* USE_TIMER_CREATE is usually disabled for Solaris. In fact it is
   supported well on this OS, but requires additional privilege. When
   user does not have it, then the testing configure program fails
   which results in USE_TIMER_CREATE not defined.
   On the other hand when we cross-compile, then we optimistically
   assume usage of timer_create function. The problem is that if we
   cross compile for example from i386-solaris2 to x86_64-solaris2,
   then the build fails with error like this:

ghc-stage2: timer_create: Not owner

   which happens on first ghc-stage2 invocation. So to support
   cross-compilation to Solaris we manually undefine USE_TIMER_CREATE
   here */
#undef USE_TIMER_CREATE
#endif /* solaris2_HOST_OS */

// Select the variant to use
#if defined(USE_PTHREAD_FOR_ITIMER)
#include "itimer/Pthread.c"
#elif defined(USE_TIMER_CREATE)
#include "itimer/TimerCreate.c"
#else
#include "itimer/Setitimer.c"
#endif
