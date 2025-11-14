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
 0.3 seconds.  For certain workflows, the 0.3 second delay may be too small.
 If an application must process an extended burst of short-lived requests
 occurring a couple of times a second, it may go idle for >0.3 seconds
 frequently, resulting in potentially dozens of major GCs triggered every
 minute (and resulting heavy CPU load) while the burst lasts.  Setting the
 `-I<n>` value higher will prevent this flurry of major GCs, though there is a
 danger that setting the value too high will prevent automatic GCs entirely,
 if the process never gets a chance to go idle for long enough to meet the
 larger threshold.

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

 Historically, the non-threaded runtime did not perform idle GC at all, and
 thus implemented deadlock detection differently. See Note [Deadlock detection]
 for the current and historical designs.


Note [Deadlock detection]
~~~~~~~~~~~~~~~~~~~~~~~~~

For the purpose of this explanation we define:
 * a /partial deadlock/ to be a set of threads that are deadlocked; and
 * a /system deadlock/ is when all threads are deadlocked.

Obviously, we can have a partial deadlock without having a system
deadlock. The design goal of deadlock detection is to guarantee to
detect (and resolve) system deadlock, but to also try to detect (and
resolve) partial deadlocks.

There are two designs that the RTS has used for deadlock detection: a
simple historical design originally used in the non-threaded RTS and a
modern design for the threaded RTS. These days we use the modern design
in both the threaded and non-threaded RTS.

A high level way to think about the two designs is as follows:
 1. the historical design looks for situations in which there *must* be
    a system deadlock; whereas
 2. the modern design looks for partial deadlocks opportunistically,
    with the guarantee that if the overall system is deadlocked that we
    will *eventually* detect this.

An advantage of the historical design is that it will detect system
deadlock promptly. A disadvantage is that it will never detect a
partial deadlock (that isn't also a system deadlock).

The modern design can detect partial deadlock, but it is not guaranteed
to detect system deadlock promptly, just eventually.

The mechanism for deadlock detection is garbage collection. GC can be
instructed to look for deadlocked threads and if it finds them to throw
exceptions to one or more threads involved in the deadlock. This
mechanism can find partial deadlocks. It is however expensive -- more
expensive than a normal major GC. So the difference in the historical
and modern designs is in when we do this expensive GC check.

The historical design
---------------------

When there was just one capability, as in the single threaded RTS, it
is possible to follow a very simple design. When there are no runnable
threads, and no threads blocked on pending I/O or on timers then there
*must* be a deadlock. And thus running deadlock detection promptly in
this situation is guaranteed to find the deadlock and wake up one or
more threads. Thus we can guarantee afterwards that there are runnable
threads.

There are a couple problems with this design, but the biggest problem
is that it cannot be extended to multiple capabilities. When there are
multiple capabilities then the fact that there are no runnable threads
on the current capability says nothing about runnable threads on other
capabilities. Runnable threads elsewhere might wake up threads on this
capability, and so there is no implication that there is a deadlock.

The other problems with this design are:
 1. it cannot find genuine deadlocks when there are any unrelated
    threads blocked on I/O or timers (see issue #26408); and
 2. it requires treating signals specially.

The problem with signals is that they're a weird kind of I/O. Threads
do not block waiting on signals. Rather signals can have handlers such
that when a signal arrives, a new thread is started to execute the
handler. This means it doesn't neatly fit into the condition "no
threads blocked on pending I/O or on timers". And if we did shoehorn it
into that definition then we would not look for deadlocks if there were
any signal handlers registered, and we would still end up with no
runnable threads after skipping deadlock detection, which violates the
post-condition that there be runnable threads. So the solution was that
after deadlock detection, if there are still no runnable threads and
there are registered signal handlers then we conclude we must wait for
a signal to be received -- which will start a thread and thus we will
end up with runnable threads. But of course this is horrible: we have
entangled two features far too tightly: deadlock detection with a weird
-- and platform specific -- kind of I/O.

The modern design
-----------------

A change of perspective is required. Instead of thinking of conditions
in which there must be a deadlock, we simply look for deadlocks in such
a way in which we will eventually find deadlocks if they exist. A
benefit of this approach is that we can find deadlocks that the simple
approach cannot. For example we can find deadlocks when there are
unrelated threads blocked on I/O or timers (see issue #26408).

The question is when to run GC in its more expensive deadlock detection
mode. We obviously do not want to do it too frequently. The design
choice is to do it during idle GC, at least sometimes. Idle GC is only
run some time after all capabilities go idle. This is a good
opportunity. We know there are no runnable threads on the capability,
so there *might* be a deadlock, and when there's nothing else to do is
also a good moment to do a more expensive GC.

The idle GC is controlled by the RecentActivity status, which
progresses through 4 stages: yes, maybe_no, inactive, done_gc. We only
invoke a deadlock-detecting major GC in the inactive state. We get into
the inactive state when:
 * the timer tick goes off
 * we were already in the maybe_no state (which itself requires no
   activity on any capability for a whole timer tick)
 * idle GC is enabled
 * it's been long enough since the most recent idle GC.
This timer tick also wakes up the I/O manager to ensue we get back to
the scheduler, and thus to scheduleDetectDeadlock.

Note that this means that deadlock detection is disabled if users
disable idle GC (by setting +RTS -I0). Historically, idle GC was not
used by default in the non-threaded RTS, but the modern design relies
on it, so it is enabled by default in all cases.

But if idle GC is enabled, then if there is a full system deadlock then
eventually we will run a major GC with deadlock detection and detect
and resolve the deadlock. It is not prompt. It must wait at least for
an idle GC, which by default is 0.3s after all capabilities go idle.

Furthermore, there is no post-condition for scheduleDetectDeadlock,
because of the non-prompt "eventually" nature of the deadlock detection
design. In particular there can still be no runnable threads. In the
threaded RTS if there's no runnable threads after this we will yield the
capability, while in the non-threaded we will ask the I/O manager to
block and wait for I/O, timers or signals.

See also Note [Deadlock detection under the nonmoving collector].
*/


/* Flag that tracks whether we have done any execution in this time
 * slice, and controls the disabling of the interval timer.
 *
 * The timer interrupt transitions ACTIVITY_YES into
 * ACTIVITY_MAYBE_NO, waits for RtsFlags.GcFlags.idleGCDelayTime,
 * and then:
 *   - if idle GC is on, set ACTIVITY_INACTIVE and wakeUpRts()
 *   - if idle GC is off, set ACTIVITY_DONE_GC and pauseTimer()
 *
 * If the scheduler finds ACTIVITY_INACTIVE, then it sets
 * ACTIVITY_DONE_GC, performs the GC and calls pauseTimer().
 *
 * If the scheduler finds ACTIVITY_DONE_GC and it has a thread to run,
 * it enables the timer again with unpauseTimer().
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
              wakeUpRts();
              // The scheduler will call pauseTimer() when it has done
              // the GC.
          } else {
              setRecentActivity(ACTIVITY_DONE_GC);
              // disable timer signals (see #1623, #5991, #9105)
              // but only if we're not profiling (e.g. passed -h or -p RTS
              // flags). If we are profiling we need to keep the timer active
              // so that samples continue to be collected.
#if defined(PROFILING)
              if (!(RtsFlags.ProfFlags.doHeapProfile
                    || RtsFlags.CcFlags.doCostCentres)) {
                  pauseTimer();
              }
#else
              pauseTimer();
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
            unpauseTimer();
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
            pauseTimer();
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
