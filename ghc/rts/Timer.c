/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2003
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
#include "Rts.h"
#include "RtsFlags.h"
#include "Proftimer.h"
#include "Schedule.h"
#include "Timer.h"
#include "Capability.h"

#if !defined(mingw32_HOST_OS)
#include "Itimer.h"
#else
#include "win32/Ticker.h"
#endif

/* ticks left before next pre-emptive context switch */
static int ticks_to_ctxt_switch = 0;

#if defined(RTS_SUPPORTS_THREADS)
/* idle ticks left before we perform a GC */
static int ticks_to_gc = 0;
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
#ifdef PROFILING
  handleProfTick();
#endif
  if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0) {
      ticks_to_ctxt_switch--;
      if (ticks_to_ctxt_switch <= 0) {
	  ticks_to_ctxt_switch = RtsFlags.ConcFlags.ctxtSwitchTicks;
	  context_switch = 1;	/* schedule a context switch */

#if defined(RTS_SUPPORTS_THREADS)
	  /* 
	   * If we've been inactive for idleGCDelayTicks (set by +RTS
	   * -I), tell the scheduler to wake up and do a GC, to check
	   * for threads that are deadlocked.
	   */
	  switch (recent_activity) {
	  case ACTIVITY_YES:
	      recent_activity = ACTIVITY_MAYBE_NO;
	      ticks_to_gc = RtsFlags.GcFlags.idleGCDelayTicks;
	      break;
	  case ACTIVITY_MAYBE_NO:
	      if (ticks_to_gc == 0) break; /* 0 ==> no idle GC */
	      ticks_to_gc--;
	      if (ticks_to_gc == 0) {
		  ticks_to_gc = RtsFlags.GcFlags.idleGCDelayTicks;
		  recent_activity = ACTIVITY_INACTIVE;
		  blackholes_need_checking = rtsTrue;
		  /* hack: re-use the blackholes_need_checking flag */
		  threadRunnable();
		  /* ToDo: this threadRunnable only works if there's
		   * another thread (not this one) waiting to be woken up
		   */
	      }
	      break;
	  default:
	      break;
	  }
#endif
      }
  }
}

int
startTimer(nat ms)
{
#ifdef PROFILING
  initProfTimer();
#endif

  return startTicker(ms, handle_tick);
}

int
stopTimer()
{
  return stopTicker();
}
