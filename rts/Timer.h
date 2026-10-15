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
 * The timer is initialised in a paused state. Use unpauseTimer to start.
 */
Timer *newPeriodicTimer(TimerHandler handler, void *handler_arg,
                        enum TimerHandlerHint timerhint,
                        Time interval);

/* Stop the timer prior to releasing resources with freeTimer().
 * The haltTimer action is *synchronous*. When it returns the caller is
 * guaranteed that the tick action will not be called again.
 */
void haltTimer(Timer *timer);

/* Release the timer's resources. The timer must be stopped first using
 * haltTimer(), or alternatively, in the case of posix it should be called in
 * the child process after a fork().
 */
void freeTimer(Timer *timer);

/* Pause and unpause (resume) the timer.
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
