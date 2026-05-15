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
 * posix/Ticker.c and win32/Ticker.c for the platform specific parts.
 *
 * If you are looking for Itimer.c then you either file or one of the
 * platform-specific Ticker.c files.
 */

#include "rts/PosixSource.h"
#include "Rts.h"

#include "Timer.h"
#include "Proftimer.h"
#include "IdleGC.h"
#include "Ticker.h"
#include "Capability.h"
#include "RtsSignals.h"
#include "rts/EventLogWriter.h"

// See Note [No timer on wasm32]
#if !defined(wasm32_HOST_ARCH)
#define HAVE_PREEMPTION
#endif

// This global counter is used to allow multiple threads to stop the
// timer temporarily with a stopTimer()/startTimer() pair.  If
//      timer_enabled  == 0          timer is enabled
//      timer_disabled == N, N > 0   timer is disabled by N threads
// When timer_enabled makes a transition to 0, we enable the timer,
// and when it makes a transition to non-0 we disable it.

static StgWord timer_disabled;

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
  handleProfTick();
  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0
      && SEQ_CST_LOAD_ALWAYS(&timer_disabled) == 0)
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
          flushEventLog(NULL);
      }
  }
#endif

  handleIdleGcTick();
}

void
initTimer(void)
{
#if defined(HAVE_PREEMPTION)
    initProfTimer();
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        initTicker(RtsFlags.MiscFlags.tickInterval, handle_tick);
    }
    SEQ_CST_STORE_ALWAYS(&timer_disabled, 1);
#endif
}

void
startTimer(void)
{
#if defined(HAVE_PREEMPTION)
    if (SEQ_CST_SUB_ALWAYS(&timer_disabled, 1) == 0) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            startTicker();
        }
    }
#endif
}

void
stopTimer(void)
{
#if defined(HAVE_PREEMPTION)
    if (SEQ_CST_ADD_ALWAYS(&timer_disabled, 1) == 1) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            stopTicker();
        }
    }
#endif
}

void
exitTimer (bool wait)
{
#if defined(HAVE_PREEMPTION)
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        exitTicker(wait);
    }
#endif
}
