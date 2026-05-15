/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2005
 *
 * Idle GC: performing a GC during idle time.
 *
 * ---------------------------------------------------------------------------*/

#include "BeginPrivate.h"

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
 * Locks required  : Transition from MAYBE_NO to INACTIVE
 * happens in the timer signal, so it is atomic.  Transition from
 * INACTIVE to DONE_GC happens under sched_mutex.  No lock required
 * to set it to ACTIVITY_YES.
 *
 * N.B. we must always use atomics here, even in the non-threaded runtime,
 * since the timer tick runs in a separate thread.
 */
extern StgWord recent_activity;

INLINE_HEADER enum RecentActivity
setRecentActivity(enum RecentActivity new_value)
{
    StgWord old = SEQ_CST_XCHG_ALWAYS((StgPtr) &recent_activity, (StgWord) new_value);
    return (enum RecentActivity) old;
}

INLINE_HEADER enum RecentActivity
getRecentActivity(void)
{
    return (enum RecentActivity) RELAXED_LOAD_ALWAYS(&recent_activity);
}

#include "EndPrivate.h"
