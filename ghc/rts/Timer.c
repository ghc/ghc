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

#if !defined(mingw32_HOST_OS)
#include "Itimer.h"
#else
#include "win32/Ticker.h"
#endif

/* ticks left before next pre-emptive context switch */
static int ticks_to_ctxt_switch = 0;

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
