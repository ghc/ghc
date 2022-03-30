# Check for a working timer_create().  We need a pretty detailed check
# here, because there exist partially-working implementations of
# timer_create() in certain versions of Linux (see bug #1933).
#
AC_DEFUN([FP_CHECK_TIMER_CREATE],[
AC_CHECK_FUNC([timer_create],[HAVE_timer_create=yes],[HAVE_timer_create=no])

if test "$HAVE_timer_create" = "yes"
then
  if test "$cross_compiling" = "yes"
  then
    # We can't test timer_create when we're cross-compiling, so we
    # optimistiaclly assume that it actually works properly.
    AC_DEFINE([USE_TIMER_CREATE], 1,  [Define to 1 if we can use timer_create(CLOCK_REALTIME,...)])
  else
  AC_CACHE_CHECK([for a working timer_create(CLOCK_REALTIME)],
    [fptools_cv_timer_create_works],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdio.h>
#if defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif
#if defined(HAVE_TIME_H)
#include <time.h>
#endif
#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

static volatile int tock = 0;
static void handler(int i)
{
   tock = 1;
}

static void timeout(int i)
{
  // timer_settime() has been known to hang, so just in case
  // we install a 1-second timeout (see #2257)
  exit(99);
}

int main(int argc, char *argv[])
{

    struct sigevent ev;
    timer_t timer;
    struct itimerspec it;
    struct sigaction action;
    int m,n,count = 0;

    ev.sigev_notify = SIGEV_SIGNAL;
    ev.sigev_signo  = SIGVTALRM;

    action.sa_handler = handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGVTALRM, &action, NULL) == -1) {
        fprintf(stderr,"SIGVTALRM problem\n");
        exit(3);
    }

    action.sa_handler = timeout;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGALRM, &action, NULL) == -1) {
      fprintf(stderr,"SIGALRM problem\n");
      exit(3);
    }
    alarm(1);

    if (timer_create(CLOCK_REALTIME, &ev, &timer) != 0) {
        fprintf(stderr,"No CLOCK_REALTIME timer\n");
        exit(2);
    }

    tock = 0;

    it.it_value.tv_sec = 0;
    it.it_value.tv_nsec = 1000000; // 1ms
    it.it_interval = it.it_value;
    if (timer_settime(timer, 0, &it, NULL) != 0) {
        fprintf(stderr,"settime problem\n");
        exit(4);
    }

    // some environments have coarse scheduler/timer granularity of ~10ms and worse
    usleep(100000); // 100ms

    if (!tock) {
        fprintf(stderr,"no CLOCK_REALTIME signal\n");
        exit(5);
    }

    timer_delete(timer);

    exit(0);
}
     ]])],
     [fptools_cv_timer_create_works=yes],
     [fptools_cv_timer_create_works=no],
     [])
  ])
case $fptools_cv_timer_create_works in
    yes) AC_DEFINE([USE_TIMER_CREATE], 1,
                   [Define to 1 if we can use timer_create(CLOCK_REALTIME,...)]);;
esac
  fi
fi
])
