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


static void handleProfTimerTick(void *unused STG_UNUSED)
{
    handleProfTick();   //TODO: short-cut this indirection.
}

static void handleCtxSwitchTimerTick(void *unused STG_UNUSED)
{
  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0)
  {
      ticks_to_ctxt_switch--;
      if (ticks_to_ctxt_switch <= 0) {
          ticks_to_ctxt_switch = RtsFlags.ConcFlags.ctxtSwitchTicks;
          contextSwitchAllCapabilities(); /* schedule a context switch */
      }
  }
}

#if defined(THREADED_RTS)
static void handleEventLogTimerTick(void *unused STG_UNUSED)
{
  if (eventLogStatus() == EVENTLOG_RUNNING
      && RtsFlags.TraceFlags.eventlogFlushTicks > 0) {
      ticks_to_eventlog_flush--;
      if (ticks_to_eventlog_flush <= 0) {
          ticks_to_eventlog_flush = RtsFlags.TraceFlags.eventlogFlushTicks;
          flushEventLog(NULL);  // Bad or worse: see issue #27250.
      }
  }
}
#endif

#if defined(THREADED_IDLEGC)
static void handleIdleGcTimerTick(void *unused STG_UNUSED)
{
    handleIdleGcTick();  //TODO: short-cut this indirection.
}
#endif

static Timer *prof_timer      = NULL;
static Timer *ctxswitch_timer = NULL;
static Timer *eventlog_timer  = NULL;
static Timer *idlegc_timer    = NULL;

void initTimers(void)
{
#if defined(HAVE_PREEMPTION)
    initProfTimer();

    /* initTimer is called in the child of a fork */
    if (prof_timer) {
        reinitTimer(prof_timer);
    } else if (RtsFlags.MiscFlags.tickInterval > 0) {
        prof_timer = newPeriodicTimer(handleProfTimerTick, NULL,
                                      TimerHandlerLongRunning,
                                      RtsFlags.MiscFlags.tickInterval);
    }

    if (ctxswitch_timer) {
        reinitTimer(ctxswitch_timer);
    } else if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0) {
        ctxswitch_timer = newPeriodicTimer(handleCtxSwitchTimerTick, NULL,
                                           TimerHandlerShortRunning,
                                           RtsFlags.MiscFlags.tickInterval);
    }

#if defined(THREADED_RTS)
    if (eventlog_timer) {
        reinitTimer(eventlog_timer);
    } else if (RtsFlags.TraceFlags.eventlogFlushTicks > 0) {
        eventlog_timer = newPeriodicTimer(handleEventLogTimerTick, NULL,
                                          TimerHandlerLongRunning,
                                          RtsFlags.MiscFlags.tickInterval);
    }
#endif

#if defined(THREADED_IDLEGC)
    if (idlegc_timer) {
        reinitTimer(idlegc_timer);
    } else if (RtsFlags.MiscFlags.tickInterval > 0) {
        idlegc_timer = newPeriodicTimer(handleIdleGcTimerTick, NULL,
                                        TimerHandlerLongRunning,
                                        RtsFlags.MiscFlags.tickInterval);
    }
#endif

#endif
}

void startTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (prof_timer) {
        runTimer(prof_timer);
    }
    if (ctxswitch_timer) {
        runTimer(ctxswitch_timer);
    }
    if (eventlog_timer) {
        runTimer(eventlog_timer);
    }
    if (idlegc_timer) {
        runTimer(idlegc_timer);
    }
#endif
}

void pauseTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (prof_timer) {
        pauseTimer(prof_timer);
    }
    if (ctxswitch_timer) {
        pauseTimer(ctxswitch_timer);
    }
    if (eventlog_timer) {
        pauseTimer(eventlog_timer);
    }
    if (idlegc_timer) {
        pauseTimer(idlegc_timer);
    }
#endif
}

void unpauseTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (prof_timer) {
        unpauseTimer(prof_timer);
    }
    if (ctxswitch_timer) {
        unpauseTimer(ctxswitch_timer);
    }
    if (eventlog_timer) {
        unpauseTimer(eventlog_timer);
    }
    if (idlegc_timer) {
        unpauseTimer(idlegc_timer);
    }
#endif
}

void exitTimers(void)
{
#if defined(HAVE_PREEMPTION)
    if (prof_timer) {
        haltTimer(prof_timer);
        freeTimer(prof_timer);
    }
    if (ctxswitch_timer) {
        haltTimer(ctxswitch_timer);
        freeTimer(ctxswitch_timer);
    }
    if (eventlog_timer) {
        haltTimer(eventlog_timer);
        freeTimer(eventlog_timer);
    }
    if (idlegc_timer) {
        haltTimer(idlegc_timer);
        freeTimer(idlegc_timer);
    }
#endif
}
