/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Interval timer service for profiling and pre-emptive scheduling.
 *
 * ---------------------------------------------------------------------------*/

/*
 * The interval timer is used for profiling and for context switching in the
 * threaded build. 
 *
 * This file defines the platform-independent view of interval timing, relying
 * on platform-specific services to install and run the timers.
 *
 */

#include "PosixSource.h"
#include "Rts.h"

#include "Timer.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "Ticker.h"
#include "Capability.h"
#include "RtsSignals.h"

/* ticks left before next pre-emptive context switch */
static int ticks_to_ctxt_switch = 0;

/* idle ticks left before we perform a GC */
static int ticks_to_gc = 0;

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
  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0) {
      ticks_to_ctxt_switch--;
      if (ticks_to_ctxt_switch <= 0) {
	  ticks_to_ctxt_switch = RtsFlags.ConcFlags.ctxtSwitchTicks;
          contextSwitchAllCapabilities(); /* schedule a context switch */
      }
  }

  /*
   * If we've been inactive for idleGCDelayTime (set by +RTS
   * -I), tell the scheduler to wake up and do a GC, to check
   * for threads that are deadlocked.
   */
  switch (recent_activity) {
  case ACTIVITY_YES:
      recent_activity = ACTIVITY_MAYBE_NO;
      ticks_to_gc = RtsFlags.GcFlags.idleGCDelayTime /
                    RtsFlags.MiscFlags.tickInterval;
      break;
  case ACTIVITY_MAYBE_NO:
      if (ticks_to_gc == 0) {
          if (RtsFlags.GcFlags.doIdleGC) {
              recent_activity = ACTIVITY_INACTIVE;
#ifdef THREADED_RTS
              wakeUpRts();
              // The scheduler will call stopTimer() when it has done
              // the GC.
#endif
          } else {
              recent_activity = ACTIVITY_DONE_GC;
              // disable timer signals (see #1623, #5991)
              // but only if we're not profiling
#ifndef PROFILING
              stopTimer();
#endif
          }
      } else {
          ticks_to_gc--;
      }
      break;
  default:
      break;
  }
}

// This global counter is used to allow multiple threads to stop the
// timer temporarily with a stopTimer()/startTimer() pair.  If 
//      timer_enabled  == 0          timer is enabled
//      timer_disabled == N, N > 0   timer is disabled by N threads
// When timer_enabled makes a transition to 0, we enable the timer,
// and when it makes a transition to non-0 we disable it.

static StgWord timer_disabled;

void
initTimer(void)
{
    initProfTimer();
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        initTicker(RtsFlags.MiscFlags.tickInterval, handle_tick);
    }
    timer_disabled = 1;
}

void
startTimer(void)
{
    if (atomic_dec(&timer_disabled) == 0) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            startTicker();
        }
    }
}

void
stopTimer(void)
{
    if (atomic_inc(&timer_disabled) == 1) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            stopTicker();
        }
    }
}

void
exitTimer (rtsBool wait)
{
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        exitTicker(wait);
    }
}
