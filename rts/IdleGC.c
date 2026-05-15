/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Idle GC: tracking of when to perform a GC during idle time.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "IdleGC.h"
#include "Timer.h"
#include "Schedule.h"

/*
 Note [GC During Idle Time]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~
 A major GC can be performed during idle time (i.e. when no Haskell threads
 are ready to run).  This can be beneficial for two reasons.  First, running
 the GC during idle time makes it less likely that a GC will be triggered when
 the process is active, increasing apparent responsiveness. Second, these idle
 time GCs allow finalizers to run, preventing resources from being held
 indefinitely when the process is otherwise idle for extended periods.

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


/* Flag that tracks whether we have done any execution in this time
 * slice, and controls the disabling of the interval timer.
 *
 * The timer interrupt transitions ACTIVITY_YES into
 * ACTIVITY_MAYBE_NO, waits for RtsFlags.GcFlags.idleGCDelayTime,
 * and then:
 *   - if idle GC is on, set ACTIVITY_INACTIVE and wakeUpRts()
 *   - if idle GC is off, set ACTIVITY_DONE_GC and stopTimer()
 *
 * If the scheduler finds ACTIVITY_INACTIVE, then it sets
 * ACTIVITY_DONE_GC, performs the GC and calls stopTimer().
 *
 * If the scheduler finds ACTIVITY_DONE_GC and it has a thread to run,
 * it enables the timer again with startTimer().
 */
enum RecentActivity {
    // the RTS is active
    ACTIVITY_YES      = 0,
    // no activity since the last timer signal
    ACTIVITY_MAYBE_NO = 1,
    // RtsFlags.GcFlags.idleGCDelayTime has passed with no activity
    ACTIVITY_INACTIVE = 2,
    // like ACTIVITY_INACTIVE, but we've done a GC too (if idle GC is
    // enabled) and the interval timer is now turned off.
    ACTIVITY_DONE_GC  = 3,
};

/* Recent activity flag.
 *
 * Note we must always use atomic operations for it, even in the non-threaded
 * runtime, since the timer tick runs in a separate thread.
 *
 * We don't need an initIdleGc. All it would do is set
 * recent_activity = ACTIVITY_YES; which we can do statically.
 */
StgWord recent_activity = ACTIVITY_YES;

static inline enum RecentActivity
setRecentActivity(enum RecentActivity new_value)
{
    StgWord old = SEQ_CST_XCHG_ALWAYS((StgPtr) &recent_activity, (StgWord) new_value);
    return (enum RecentActivity) old;
}

static inline enum RecentActivity
getRecentActivity(void)
{
    return (enum RecentActivity) RELAXED_LOAD_ALWAYS(&recent_activity);
}

bool isIdleGcPending(void)
{
    return (getRecentActivity() == ACTIVITY_INACTIVE);
}

/* - countdown for minimum idle time before we start a GC (set by -I) */
static int idle_ticks_to_gc = 0;

/* - countdown for minimum time *between* idle GCs (set by -Iw) */
static int inter_gc_ticks_to_gc = 0;

/*
 * Called from handle_tick().
 */
void handleIdleGcTick(void)
{
  /*
   * If we've been inactive for idleGCDelayTime (set by +RTS
   * -I), tell the scheduler to wake up and do a GC, to check
   * for threads that are deadlocked.  However, ensure we wait
   * at least interIdleGCWait (+RTS -Iw) between idle GCs.
   */
  switch (getRecentActivity()) {
  case ACTIVITY_YES:
      setRecentActivity(ACTIVITY_MAYBE_NO);
      idle_ticks_to_gc = RtsFlags.GcFlags.idleGCDelayTime /
                         RtsFlags.MiscFlags.tickInterval;
      break;
  case ACTIVITY_MAYBE_NO:
      if (idle_ticks_to_gc == 0 && inter_gc_ticks_to_gc == 0) {
          if (RtsFlags.GcFlags.doIdleGC) {
              setRecentActivity(ACTIVITY_INACTIVE);
              inter_gc_ticks_to_gc = RtsFlags.GcFlags.interIdleGCWait /
                                     RtsFlags.MiscFlags.tickInterval;
#if defined(THREADED_RTS)
              wakeUpRts();
              // The scheduler will call pauseTimer() when it has done
              // the GC.
#endif
          } else {
              setRecentActivity(ACTIVITY_DONE_GC);
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

void notifyIdleGcActive(void)
{
    switch (getRecentActivity())
    {
    case ACTIVITY_DONE_GC: {
        // ACTIVITY_DONE_GC means we turned off the timer signal to
        // conserve power (see #1623).  Re-enable it here.
        uint32_t prev;
        prev = setRecentActivity(ACTIVITY_YES);
        if (prev == ACTIVITY_DONE_GC) {
#if !defined(PROFILING)
            startTimer();
#endif
        }
        break;
    }
    case ACTIVITY_INACTIVE:
        // If we reached ACTIVITY_INACTIVE, then don't reset it until
        // we've done the GC.  The thread running here might just be
        // the IO manager thread that handle_tick() woke up via
        // wakeUpRts().
        break;
    default:
        setRecentActivity(ACTIVITY_YES);
    }
}

void notifyIdleGcDone(bool force_major)
{
    switch (getRecentActivity()) {
    case ACTIVITY_INACTIVE:
        if (force_major) {
            // We are doing a GC because the system has been idle for a
            // timeslice and we need to check for deadlock.  Record the
            // fact that we've done a GC and turn off the timer signal;
            // it will get re-enabled if we run any threads after the GC.
            setRecentActivity(ACTIVITY_DONE_GC);
#if !defined(PROFILING)
            stopTimer();
#endif
            break;
        }
        // fall through...

    case ACTIVITY_MAYBE_NO:
        // the GC might have taken long enough for the timer to set
        // recent_activity = ACTIVITY_MAYBE_NO or ACTIVITY_INACTIVE,
        // but we aren't necessarily deadlocked:
        setRecentActivity(ACTIVITY_YES);
        break;

    case ACTIVITY_DONE_GC:
        // If we are actually active, the scheduler will reset the
        // recent_activity flag and re-enable the timer.
        break;
    case ACTIVITY_YES:
        break;
    }
}
