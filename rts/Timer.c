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


/*
 Note [GC During Idle Time]
 --------------------------

 In the threaded RTS, a major GC can be performed during idle time (i.e., when
 no Haskell computations are ready to run).  This can be beneficial for two
 reasons.  First, running the GC during idle time makes it less likely that a GC
 will be triggered when the process is active, increasing apparent
 responsiveness.  Second, these idle time GCs allow finalizers to run,
 preventing resources from being held indefinitely when the process is otherwise
 idle for extended periods.

 There are two runtime RTS options to control idle time GC timing.  The primary
 control is set by `-I<n>`, which specifies the minimum period of time the
 process must be idle before a GC is automatically triggered.  It defaults to
 0.3 seconds for the threaded runtime and 0 (which disables idle GC entirely)
 for the non-threaded runtime.  For certain workflows, the 0.3 second delay for
 the threaded RTS may be too small.  If an application must process an extended
 burst of short-lived requests occurring a couple of times a second, it may go
 idle for >0.3 seconds frequently, resulting in potentially dozens of major
 GCs triggered every minute (and resulting heavy CPU load) while the burst
 lasts.  Setting the `-I<n>` value higher will prevent this flurry of major GCs,
 though there is a danger that setting the value too high will prevent automatic
 GCs entirely, if the process never gets a chance to go idle for long enough to
 meet the larger threshold.

 In this case, the second control, set by `-Iw<n>` may be helpful.  For example,
 setting `-I0.3 -Iw30` triggers automatic GCs after only 0.3 seconds of idle
 time, but subject to a minimum delay *between* automatic GCs of at least 30
 seconds.  This is likely to work well for applications that must process a
 nearly constant stream of frequent, short-lived requests, ensuring that
 automatic GCs are triggered promptly when the process goes idle while limiting
 the overall frequency of such GCs.  (The default is `-Iw0`, meaning no limit on
 frequency of GCs.)

 Automatic GC timing is implemented below using two count-down timers.  The
 `idle_ticks_to_gc` timer counts down the `-I<n>` setting: it is initialized
 when the process goes idle and counts down idle time before an automatic GC
 becomes possible.  The `inter_gc_ticks_to_gc` counts down the `-Iw<n>` setting:
 it is initialized when an automatic GC is actually performed, and holds off the
 next automatic GC until its count expires, limiting the overall frequency of
 automatic GCs.  Both timers must expire before an automatic GC is triggered.

 See issue #11134 for additional detail.
*/

/* - countdown for minimum idle time before we start a GC (set by -I) */
static int idle_ticks_to_gc = 0;

/* - countdown for minimum time *between* idle GCs (set by -Iw) */
static int inter_gc_ticks_to_gc = 0;

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
   * for threads that are deadlocked.  However, ensure we wait
   * at least interIdleGCWait (+RTS -Iw) between idle GCs.
   */
  switch (recent_activity) {
  case ACTIVITY_YES:
      recent_activity = ACTIVITY_MAYBE_NO;
      idle_ticks_to_gc = RtsFlags.GcFlags.idleGCDelayTime /
                         RtsFlags.MiscFlags.tickInterval;
      break;
  case ACTIVITY_MAYBE_NO:
      if (idle_ticks_to_gc == 0 && inter_gc_ticks_to_gc == 0) {
          if (RtsFlags.GcFlags.doIdleGC) {
              recent_activity = ACTIVITY_INACTIVE;
              inter_gc_ticks_to_gc = RtsFlags.GcFlags.interIdleGCWait /
                                     RtsFlags.MiscFlags.tickInterval;
#if defined(THREADED_RTS)
              wakeUpRts();
              // The scheduler will call stopTimer() when it has done
              // the GC.
#endif
          } else {
              recent_activity = ACTIVITY_DONE_GC;
              // disable timer signals (see #1623, #5991, #9105)
              // but only if we're not profiling (e.g. passed -h or -p RTS
              // flags). If we are profiling we need to keep the timer active
              // so that samples continue to be collected.
#if defined(PROFILING)
              if (!(RtsFlags.ProfFlags.doHeapProfile
                    || RtsFlags.CcFlags.doCostCentres)) {
                  stopTimer();
              }
#else
              stopTimer();
#endif
          }
      } else {
          if (idle_ticks_to_gc) idle_ticks_to_gc--;
          if (inter_gc_ticks_to_gc) inter_gc_ticks_to_gc--;
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
    if (atomic_inc(&timer_disabled, 1) == 1) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            stopTicker();
        }
    }
}

void
exitTimer (bool wait)
{
    if (RtsFlags.MiscFlags.tickInterval != 0) {
        exitTicker(wait);
    }
}
