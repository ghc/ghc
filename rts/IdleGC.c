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
 the overall frequency of such GCs.  The default is `-Iw0`, meaning no limit on
 frequency of GCs.  See issue #11134 for additional detail.

 The idle GC is also the only occasion when deadlock detection is performed.
 So note that disabling idle GC with RTS flag `-I0` will also disable deadlock
 detection.

TODO: refer to the deadlock detection note for further details, once that note exists.
*/

/*
 Note [Design of idle GC tracking]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The tracking of when to perform idle GC relies on the timer tick and a small
 number of hooks into the scheduler.  The idle GC is actually invoked by the
 scheduler.  The design of idle GC tracking is slightly different between the
 threaded and non-threaded RTS ways.

 The idle GC tracking makes use of a small state machine:
 * Active: threads are running, or have recently been running, on one or more
   capabilities. The timer thread is running.
 * Pending: all capabilities have been idle for long enough that idle GC should
   be performed and has not yet completed. The timer thread is paused.
 * Done: an idle GC has completed and all capabilities remain idle. The timer
   thread is paused.

 When idle GC is disabled, only the Active and Done states are used, and the
 only responsibility is to pause/unpause the timer thread.

 We use a few shared state variables:
 * shared_idlegc_state: an enum indicating the current state machine state;
 * shared_current_tick: a tick counter, updated by the timer tick;
 * shared_last_idlegc_tick: a tick timestamp to keep track of when the most
   recent idle GC was completed;
 * cap->idle_since_tick: a per-cabability timestamp, to track if the capability
   is idle and if so, the timestamp (as a tick) when it became idle.

 All the shared state variables above are accessed from multiple threads. The
 use of the shared variables is intended to be resonable performant: to avoid
 frequent mutation from multiple threads, or expensive memory operations (like
 cas or atomic exchange). All the accesses use atomic operations with relaxed
 memory order. This should be kept in mind when reviewing or modifying the
 implementation: all reads can be somewhat stale, and the variables need not
 be consistent with each other (no ordering is currently used).

 We use a couple constants initialised at RTS startup:
 * idlegc_delay_ticks: the value of the `-I<n>` RTS flag, but viewed as a
   number of ticks;
 * inter_idlegc_delay_ticks: the value of the `-Iw<n>` RTS flag, but viewed as
   a number of ticks.

 In the threaded and non-threaded RTS
 ------------------------------------
 * The ticker thread increments the shared_current_tick every tick.
   See handleIdleGcTick().
 * Whenever the scheduler on a capability has a thread to run it resets the
   cap->idle_since_tick to 0. See notifyIdleGcCapabilityIsActive().
 * Whenever the scheduler on a capability is about to go idle, it sets the
   cap->idle_since_tick to the shared_current_tick.
   See notifyIdleGcCapabilityIsIdle().
 * Whenever the scheduler on a capability completes an idle GC, it sets the
   shared_last_idlegc_tick to the shared_current_tick.

 In the threaded RTS
 -------------------
 We start in the Active state.

 In the active state:
   In the timer tick, we check if all capabilities have been idle for at least
   idlegc_delay_ticks, and the time since the last idle GC is at least
   inter_idlegc_delay_ticks. If so, we transition to the pending state. We also
   wake up at least one capability, to run the scheduler, using wakeUpRts().

 In the pending state:
   In the scheduler on some capability, before blocking, initiates an idle GC.
   Upon completion, transition to the done state. Pause the timer thread.
   Alternatively, if a thread becomes runnable then transition back to the
   active state.

 In the done state:
   An idle GC has completed. The timer thread is paused. If any capability
   wakes up and runs a Haskell thread then transition to the active state and
   unpause the timer.


 In the non-threaded RTS
 -----------------------

 In the non-threaded RTS, when the scheduler has no threads to run and so is
 about to wait (for I/O, timeouts, signals etc) we calculate a maximum time
 that it should wait (based on the idleGCDelayTime). Afterwards if it did wait
 the maximum wait time then we set state to signal that idle GC is pending.
 After the scheduler completes an idle GC the pending state it reset and the
 timer tick is paused.

 The progression goes through the following state changes:

 1. While active, the scheduler finds it has no threads to run.
    It waits for events,

 In particular, the design
 for the threaded RTS makes use of the timer tick, while the non-threaded
 design is driven by scheduler activity and timeouts.

The following RTS flags affect the behaviour. See Note [GC During Idle Time].
 - `-I<n>` as RtsFlags.GcFlags.idleGCDelayTime
   and the derived flag RtsFlags.GcFlags.doIdleGC = (idleGCDelayTime != 0)
 - `-Iw<n>` as RtsFlags.GcFlags.interIdleGCWait
 - `-V<n>` as RtsFlags.MiscFlags.tickInterval

 */

/* TODO: consider wrap around at ~500 day mark */
typedef int32_t TickCount;
#define CAP_NOT_IDLE (-1)

/* Constant initialised at startup to RtsFlags.GcFlags.idleGCDelayTime viewed
 * as a number of ticks.
 */
static TickCount idlegc_delay_ticks;

/* Constant initialised at startup to RtsFlags.GcFlags.interIdleGCWait viewed
 * as a number of ticks.
 */
static TickCount inter_idlegc_delay_ticks;

/* Shared variable to count the number of timer ticks that have occurred since
 * the start of the process. This gets used as a very coarse timestamp, and as
 * a reference to decide if enough ticks have elapsed to do an idle GC.
 *
 * This can and will wrap, at 2^32. This takes around 500 days at the default
 * tick interval, but could be much lower when using a high ticker frequency.
 */
static TickCount shared_current_tick = 0;

/* The current_tick at the time that the last idle GC _completed_.
 */
static TickCount shared_last_idlegc_tick = 0;

/* The Time that the ticker was paused. Used to adjust shared_current_tick
 * when the ticker is resumed.
 */
static Time shared_ticker_paused_time = 0;


enum IdleGCState {
  IDLEGC_STATE_ACTIVE,
  IDLEGC_STATE_PENDING,
  IDLEGC_STATE_DONE
};

static enum IdleGCState shared_idlegc_state;

//TODO: group these shared vars into cache lines to avoid false sharing.
// review access patterns, which threads read/write and frequency.

// * shared_idlegc_state: read by every cap during releaseCapability_

// * cap->idle_since_tick: written by thread animating the cap during
//   notifyIdleGcCapabilityIsActive and read by timer thread in handleIdleGcTick

/* forward decls */
static void pauseTimerUnlessProfiling(void);
static void unpauseTimerUnlessProfiling(void);


void initIdleGc(void)
{
    if (RtsFlags.GcFlags.doIdleGC) {
        /* Use division rounding up (a+b-1)/b, to avoid getting 0 ticks, as this
         * would give unexpected results.
         */
        idlegc_delay_ticks =
            (RtsFlags.GcFlags.idleGCDelayTime + RtsFlags.MiscFlags.tickInterval - 1)
          / RtsFlags.MiscFlags.tickInterval;

        inter_idlegc_delay_ticks =
            (RtsFlags.GcFlags.interIdleGCWait + RtsFlags.MiscFlags.tickInterval - 1)
          / RtsFlags.MiscFlags.tickInterval;

        /* The -Iw<n> parameter is supposed to control times *between* idle GCs,
         * not time since program start. So by initialising to the negative of the
         * interval then we can do an idle GC almost immediately if needed.
         */
        shared_last_idlegc_tick = -inter_idlegc_delay_ticks;
    } else {
        /* When idle GC is disabled, we turn off the timer tick after one tick
         * of all caps being idle.
         */
        idlegc_delay_ticks = 1;
        inter_idlegc_delay_ticks = 0;
    }

    debugTrace(DEBUG_idlegc,
               "Idle GC (tick %d): initialising, delay ticks = %d,"
               " inter-gc ticks = %d, tick interval = %ldus",
               RELAXED_LOAD_ALWAYS(&shared_current_tick),
               idlegc_delay_ticks, inter_idlegc_delay_ticks,
               RtsFlags.MiscFlags.tickInterval / 1000);
}

bool isIdleGcPending(void)
{
    return RELAXED_LOAD_ALWAYS(&shared_idlegc_state) == IDLEGC_STATE_PENDING;
}

/* Used by the scheduler on a capability to notify the idle GC tracking that
 * the capability is active.  It also tells us if this is the first thread run
 * after the capability wakes up from idle (false positives are ok).
 */
void notifyIdleGcCapabilityIsActive(Capability *cap, bool cap_just_awoke)
{
    /* The idle_since_tick variable is per-cap and is only written here, and
     * only read by the timer thread. It shares a cache line with many other
     * per-cap mutable variables, so there is no point in a read-before-write
     * to avoid updates here.
     */
    RELAXED_STORE_ALWAYS(&cap->idle_since_tick, CAP_NOT_IDLE);

    /* This scheduler hook is called in the hot path, for _every_ thread that
     * gets run, so we want to avoid writing to the shared variable
     * shared_idlegc_state too often. We optimise this case by:
     *  1. having the scheduler tell us if this is the first thread it is
     *     running after wakeing up (cap_just_awoke); and
     *  2. reading the variable before changing it to avoid unnecessary cache
     *     line invalidations.
     */
    if (cap_just_awoke) {
        debugTrace(DEBUG_idlegc, "Idle GC (tick %d): cap %d became active",
                                 RELAXED_LOAD_ALWAYS(&shared_current_tick),
                                 cap->no);
        enum IdleGCState state = RELAXED_LOAD_ALWAYS(&shared_idlegc_state);
        if (state != IDLEGC_STATE_ACTIVE) {
            debugTrace(DEBUG_idlegc, "Idle GC (tick %d): transition to ACTIVE",
                                     RELAXED_LOAD_ALWAYS(&shared_current_tick));
            RELAXED_STORE_ALWAYS(&shared_idlegc_state, IDLEGC_STATE_ACTIVE);
            unpauseTimerUnlessProfiling();
        }
    }
}

/* Used by the scheduler on a capability to notify the idle GC tracking that
 * the capability is about to go idle (and wait on I/O, timers etc), because
 * the capability has no threads to run.
 */
void notifyIdleGcCapabilityIsIdle(Capability *cap)
{
    TickCount current_tick = RELAXED_LOAD_ALWAYS(&shared_current_tick);
    RELAXED_STORE_ALWAYS(&cap->idle_since_tick, current_tick);

    debugTrace(DEBUG_idlegc, "Idle GC (tick %d): cap %d became idle",
                             current_tick, cap->no);
}

/* Called by the scheduler when a major GC was forced. This happens after an
 * idle GC, but can also be forced by application code, and during shutdown.
 *
 * If we had requested an idle GC and we've now completed a forced major GC
 * then we can assume we're now really idle and
 */
void notifyIdleGcCompleted(void)
{
    TickCount current_tick = RELAXED_LOAD_ALWAYS(&shared_current_tick);
    RELAXED_STORE_ALWAYS(&shared_last_idlegc_tick, current_tick);

    debugTrace(DEBUG_idlegc, "Idle GC (tick %d): completed GC",
                             current_tick);

    debugTrace(DEBUG_idlegc, "Idle GC (tick %d): transition to DONE",
                             current_tick);
    RELAXED_STORE_ALWAYS(&shared_idlegc_state, IDLEGC_STATE_DONE);
}

/*
 * Called from handle_tick().
 */
void handleIdleGcTick(void)
{
    /* Increment the tick counter. This is _not_ an atomic increment, but this
     * thread is the only writer so that's ok and is much cheaper.
     *
     * It is ok for other threads to read a value that is somewhat out of date.
     * It just means we'll do idle GC slightly sooner than otherwise. The
     * orders of magnitude work in our favour here: memory/cache updates are
     * relatively quick (1e-7 sec) compared to the typical tick time (1e-2 sec).
     */
    TickCount current_tick = RELAXED_LOAD_ALWAYS(&shared_current_tick);
    RELAXED_STORE_ALWAYS(&shared_current_tick, current_tick+1);

    /* Now we want to check if _all_ the capabilities have been idle for long
     * enough that it is time to request an idle GC.
     *
     * First, we can short-cut if the time duration since the last idle GC is
     * less than the delay duration required between idle GCs.
     *
     * Note however that if we short-cut here then we do *not* pause the ticker,
     * otherwise handleIdleGcTick will not get called at all and we would not
     * have an opportunity to invoke idle GC later. This means the ticker
     * remains active even if all caps are idle up until the inter-idle-gc
     * delay time. See issue #TODO for further details.
     */
    if (inter_idlegc_delay_ticks) {
        TickCount last_idlegc_tick =
            RELAXED_LOAD_ALWAYS(&shared_last_idlegc_tick);
        //FIXME: consider current_tick wraparound
        if (current_tick - last_idlegc_tick < inter_idlegc_delay_ticks) return;
    }

    /* Next, we will look at each capability. Each capability has an
     * idle_since_tick. If the capability is not idle we can short-cut. We
     * compare the time when the cap went idle, plus the idle gc delay duration
     * with the current tick. If it's at or after the current tick then we
     * continue to the remaining capabilities, otherwise we can short-cut.
     */
    int N = getNumCapabilities();
    for (int i = 0; i < N; i++) {
        Capability *cap_i = getCapability(i);
        TickCount idle_since_tick = RELAXED_LOAD_ALWAYS(&cap_i->idle_since_tick);
        if ((idle_since_tick == CAP_NOT_IDLE) ||
            (current_tick - idle_since_tick < idlegc_delay_ticks)) {
            return;
        }
    }
    /* At this point we know that all caps have been idle for
     * idlegc_delay_ticks or more. */
    debugTrace(DEBUG_idlegc, "Idle GC (tick %d): all caps have been idle for "
                             "at least %d ticks",
                             current_tick, idlegc_delay_ticks);

    /* Note that because pauseTimer is async, it is possible that we get extra
     * ticks, and thus can get to this point even once we are in the pending or
     * done states. Though actually the actions below are idempotent, it is a
     * simpler design to avoid relying on this and to test for being in a
     * non-active state. Testing for this is reliable because we set
     * shared_idlegc_state to non-active states only in this thread.
     */
    if (RELAXED_LOAD_ALWAYS(&shared_idlegc_state) != IDLEGC_STATE_ACTIVE) {
        return;
    }

    /* Transition to pending or done, depending on whether we're doing idle GC.
     * If we are doing idle GC, wake up at least one capability to return to
     * the scheduler to initiate the idle GC.
     */
    if (RtsFlags.GcFlags.doIdleGC) {
        debugTrace(DEBUG_idlegc, "Idle GC (tick %d): transition to PENDING",
                   RELAXED_LOAD_ALWAYS(&shared_current_tick));
        RELAXED_STORE_ALWAYS(&shared_idlegc_state, IDLEGC_STATE_PENDING);
#if defined(THREADED_RTS)
        wakeUpRts();
#endif
    } else {
        debugTrace(DEBUG_idlegc, "Idle GC (tick %d): transition to DONE",
                   RELAXED_LOAD_ALWAYS(&shared_current_tick));
        RELAXED_STORE_ALWAYS(&shared_idlegc_state, IDLEGC_STATE_DONE);
    }

    /* Transitioning out of the active state so pause the timer. */
    pauseTimerUnlessProfiling();
}

/* Pause the timer thread (see #1623, #5991, #9105) but only if we're not
 * profiling (e.g. passed -h or -p RTS flags). If we are profiling we need
 * to keep the timer active so that samples continue to be collected.
 */
static void pauseTimerUnlessProfiling(void)
{
#if defined(PROFILING)
    if (!(RtsFlags.ProfFlags.doHeapProfile || RtsFlags.CcFlags.doCostCentres))
#endif
    {
        debugTrace(DEBUG_idlegc, "Idle GC (tick %d): pausing the ticker",
                                 RELAXED_LOAD_ALWAYS(&shared_current_tick));
        stopTimer();

        /* Save the time at which we pause, so we can account for ticks missed
         * while the ticker was paused.
         */
        Time paused_time = getProcessElapsedTime();
        RELAXED_STORE_ALWAYS(&shared_ticker_paused_time, paused_time);
    }
}

static void unpauseTimerUnlessProfiling(void)
{
#if defined(PROFILING)
    if (!(RtsFlags.ProfFlags.doHeapProfile || RtsFlags.CcFlags.doCostCentres))
#endif
    {
        /* Adjust the shared_current_tick by (an approximation of) the number
         * of ticks that were missed while the ticker was paused.
         */
        Time paused_time = RELAXED_LOAD_ALWAYS(&shared_ticker_paused_time);
        Time unpaused_time = getProcessElapsedTime();
        TickCount paused_ticks = (unpaused_time - paused_time)
                               / RtsFlags.MiscFlags.tickInterval;
        TickCount current_tick = RELAXED_LOAD_ALWAYS(&shared_current_tick)
                               + paused_ticks;
        RELAXED_STORE_ALWAYS(&shared_current_tick, current_tick);

        startTimer();

        debugTrace(DEBUG_idlegc, "Idle GC (tick %d): unpausing the ticker "
                                 "after %d ticks paused",
                                 current_tick, paused_ticks);
    }
}

