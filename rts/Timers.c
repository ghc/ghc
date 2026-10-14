/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Interval timer service for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * The interval timer is used for profiling and for context switching.
 *
 * This file defines the platform-independent view of interval timing, relying
 * on platform-specific services to install and run the timers. See
 * posix/Timer.c and win32/Timer.c for the platform specific parts.
 *
 * If you are looking for Itimer.c then you either file or one of the
 * platform-specific Timer.c files.
 */

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Timer.h"
#include "Timers.h"
#include "Proftimer.h"
#include "IdleGC.h"
#include "Capability.h"
#include "RtsSignals.h"
#include "rts/EventLogWriter.h"

/* ticks left before next pre-emptive context switch */
static int ticks_to_ctxt_switch = 0;

#if defined(THREADED_RTS)
/* ticks left before next next forced eventlog flush */
static int ticks_to_eventlog_flush = 0;
#endif


/*
 * Function: handle_tick()
 *
 * At each occurrence of a tick, the OS timer will invoke
 * handle_tick().
 */
static
void
handle_tick(int unused STG_UNUSED)
{
  handleProfTick(); // Bad or worse: see issue #27250.

  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0)
  {
      ticks_to_ctxt_switch--;
      if (ticks_to_ctxt_switch <= 0) {
          ticks_to_ctxt_switch = RtsFlags.ConcFlags.ctxtSwitchTicks;
          contextSwitchAllCapabilities(); /* schedule a context switch */
      }
  }
#if defined(THREADED_RTS)
  if (eventLogStatus() == EVENTLOG_RUNNING
      && RtsFlags.TraceFlags.eventlogFlushTicks > 0) {
      ticks_to_eventlog_flush--;
      if (ticks_to_eventlog_flush <= 0) {
          ticks_to_eventlog_flush = RtsFlags.TraceFlags.eventlogFlushTicks;
          flushEventLog(NULL);  // Bad or worse: see issue #27250.
      }
  }
#endif

#if defined(THREADED_IDLEGC)
  handleIdleGcTick();
#endif
}

static Timer *main_timer      = NULL;

void initTimers(void)
{
#if defined(HAVE_PREEMPTION)
    initProfTimer();

    /* initTimer is called in the child of a fork */
    if (main_timer) {
        freeTimer(main_timer);
    }

    if (RtsFlags.MiscFlags.tickInterval != 0) {
        main_timer = initTimer(RtsFlags.MiscFlags.tickInterval, handle_tick);
    }
#endif
}

/* Deprecated exported functions. Now no-ops.
 * Historically they were used by the process and unix libraries to disable
 * the signal-based interval timer, since otherwise the timer signal would
 * keep going off in the child process and confusing everything. The interval
 * timer no longer uses signals, so there is no need any more for libraries to
 * disable the timer. Also, the timer internal API has changed.
 *
 * They are however stealing the good names! We're forced to use run/halt
 * until we can get rid of these entirely.
 */
void stopTimer(void)  { /* no-op */ }
void startTimer(void) { /* no-op */ }

void pauseTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        pauseTimer(main_timer);
    }
#endif
}

void unpauseTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        unpauseTimer(main_timer);
    }
#endif
}

void exitTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        haltTimer(main_timer);
        freeTimer(main_timer);
    }
#endif
}
