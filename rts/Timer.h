/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Interface to the OS-specific implementation of periodic timers.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"


typedef struct Timer Timer; // opaque

typedef void (*TimerHandler)(void*);

enum TimerHandlerHint {
    /* The handler promises to:
     * 1. Run for a short (and certainly bounded) time. In particular this
     *    means no locks and no I/O.
     * 2. Use bounded and modest stack space. In particular this means no calls
     *    to arbitrary user code.
     *
     * This combination allows implementation(s) to save resources.
     */
    TimerHandlerShortRunning,

    /* The handler may be long running or may block, e.g. taking locks or
     * performing I/O, or may use significant stack depth.
     */
    TimerHandlerLongRunning
};


/* Create a new periodic timer, with the given handler and interval.
 *
 * A periodic timer is one that calls the tick handler every interval.
 *
 * The timer is created in the halted state. Use runTimer to start.
 * See Note [Timer run/halt and pause/unpause].
 */
Timer *newPeriodicTimer(TimerHandler handler, void *handler_arg,
                        enum TimerHandlerHint timerhint,
                        Time interval);


/* Run (start) the timer. This allocates additional resources.
 *
 * This is idempotent and asynchronous.
 *
 * See Note [Timer run/halt and pause/unpause].
 */
void runTimer(Timer *timer);


/* Halt (stop) the timer. This releases most of the timer's expensive
 * resources.
 *
 * See Note [Timer run/halt and pause/unpause].
 *
 * The haltTimer action is idempotent and *synchronous*. When it returns the
 * caller is guaranteed that the tick action will not be called again.
 *
 * It is *not* safe to call this function on a timer from within that timer's
 * own tick handler. It would deadlock.
 */
void haltTimer(Timer *timer);


/* Release the timer's remaining resources. The timer must be halted first
 * using haltTimer().
 *
 * Obviously no other timer functions may be called after this.
 */
void freeTimer(Timer *timer);

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
/* Re-initialise an existing timer in the child process after a fork(). This
 * puts the timer back into a known state, and specifically the halted state.
 *
 * The timer does not need to be halted or paused across the fork.
 */
void reinitTimer(Timer *timer);
#endif

/* Note [Timer run/halt and pause/unpause]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Timers have two independent controls:
 * + running control, set by runTimer/haltTimer; and
 * + paused control, set by pauseTimer/unpauseTimer.
 *
 * The timer handler only runs if the timer is both running _and_ not paused.
 * The controls are independent: setting one does not affect the other. Both
 * controls can be used concurrently without coordination.
 *
 * The pause/unpause is a lightweight mechanism that can be used frequently.
 * The transition actions are asynchronous and intended to be quick. This
 * should be used for the purpose of *efficiency*: to temporarily avoid
 * unnecessary OS thread wakeups caused by the timer. In particular this is
 * used (by the idle GC subsystem) when the whole RTS is idle.
 *
 * The run/halt is a more heavyweight mechanism. A timer that is halted uses
 * fewer resources. In particular it is intended to use fewer kernel resources.
 * This should be used as a coarse switch for turning a timer on at RTS
 * start-up or later during runtime, e.g. to enable profiling or eventlog
 * flushing under API control.
 *
 * These mechanisms should *not* be used for *concurrency safety*: to prevent
 * the tick action from running concurrently with some other critical section.
 * The solution to that problem is to use proper locking (or appropriate
 * atomics).
 */


/* Pause and unpause (resume) the timer.
 *
 * See Note [Timer run/halt and pause/unpause].
 *
 * The pauseTimer and unpauseTimer actions are *asynchronous*. After calling
 * pauseTimer, the timer will pause eventually, but there may be another tick
 * action before it does pause (and theoretically there could be several but
 * in practice this is unlikely). Similarly, after calling unpauseTimer the
 * timer will start up again eventually, but there is an unspecified delay
 * between the unpause and the next tick action (but in practice it is short).
 *
 * This should be used for the purpose of *efficiency*: to avoid unnecessary
 * OS thread wakeups caused by the timer.
 *
 * These should *not* be used for the purpose of *concurrency safety*: to
 * prevent the tick action from running concurrently with some other critical
 * section. The synchronous case is not provided because it is not currently
 * needed (and proper locking is often a better solution anyway).
 *
 * The pairing of unpauseTimer and the handle_tick action form a
 * synchonises-with relation: values written before unpauseTimer can be
 * read from the resulting handle_tick action.
 *
 * It *is* safe to call these functions from within the tick handler itself.
 *
 * It is safe to use these functions concurrently from multiple threads, but
 * note that they *are* idempotent. This means it is not appropriate to use
 * paired pause/unpause calls concurrently. They can be used by threads based
 * on consistent use of some shared state or observation.
 */
void pauseTimer(Timer *timer);
void unpauseTimer(Timer *timer);

#include "EndPrivate.h"
